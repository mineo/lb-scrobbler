{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad      (when)
import           Control.Monad      (liftM2)
import           Control.Monad      (liftM3)
import           Control.Monad      (ap)
import           Control.Monad      (liftM)
import           Data.Aeson         (ToJSON (..), encode, object, (.:), (.:?),
                                     (.=))
import           Data.Maybe         (fromJust, isJust)
import           Data.Text          (Text, pack)
import           Data.UUID          (UUID, fromString)
import           Data.UUID.Aeson    ()
import           GHC.Generics
import           Network.MPD        (Metadata (..), Subsystem (..), idle,
                                     sgGetTag, toString, withMPD)
import qualified Network.MPD        as MPD
import           Safe               (headMay, headNote)
import           System.Environment (lookupEnv)
import           System.Exit        (die)
import           System.Posix.Time  (epochTime)

type Token = UUID

token :: IO (Maybe Token)
token =
  liftM (\l -> case l of
    Nothing -> Nothing
    Just t -> fromString t)
  (lookupEnv "LISTENBRAINZ_TOKEN")



data Listen = Listen
  { listened_at  :: Maybe Int
  , track_name   :: String
  , artist_name  :: String
  , artist_id    :: UUID
  , recording_id :: UUID
  , release_id   :: Maybe UUID
  } deriving (Generic)

instance ToJSON Listen where
  toJSON (Listen {..}) = object
                         [ "listened_at" .= listened_at
                         , "track_metadata" .= object
                           [ "artist_name" .= (pack artist_name)
                           , "track_name" .= (pack track_name)
                           , "additional_info" .= object
                             [ "recording_id" .= recording_id
                             , "artist_id" .= artist_id
                             , "release_id" .= release_id
                             ]
                           ]
                         ]

data ListenBuild = Either String Listen

idleMPD :: IO (MPD.Response [Subsystem])
idleMPD = withMPD (idle [PlayerS])

getStatus :: IO (MPD.Response MPD.Status)
getStatus = withMPD MPD.status

getCurrentSong :: IO (MPD.Response (Maybe MPD.Song))
getCurrentSong = withMPD MPD.currentSong

isListenWorthy :: Maybe MPD.Status -> MPD.Status -> Bool
isListenWorthy oldStatus newStatus =
  if not switched_to_paused && not sameSongLaterPosition then
    case oldStatus of
      Nothing -> let pl = played_long_enough newElapsed newLength in newState == MPD.Playing && isJust pl && fromJust pl
      Just _ -> let pl = played_long_enough oldElapsed oldLength -- Check if we played the old song long enough to make it scrobbleworthy
                    rep = playing_repeatedly oldElapsed newElapsed newLength
                    val = (||) <$> pl <*> rep
                in isJust val && fromJust val
  else False
  where newState = MPD.stState newStatus
        oldState = MPD.stState <$> oldStatus
        switched_to_paused = (newState /= MPD.Playing) && (oldState == Just MPD.Playing)
        played_long_enough :: Maybe Double -> Maybe MPD.Seconds -> Maybe Bool
        played_long_enough elapsed length = do
          el <- elapsed
          l <- length
          return (el > 240 || l >= 30 && el > fromIntegral l / 2)
        -- Check if the song is playing on repeat. This doesn't work
        -- yet because prev_elapsed is from the beginning of the
        -- previous song and thus too small.
        -- TODO set up time measurement like mpdscribble does
        playing_repeatedly :: Maybe Double -> Maybe Double -> Maybe MPD.Seconds -> Maybe Bool
        playing_repeatedly prev_elapsed elapsed length = do
          pr_el <- prev_elapsed
          el <- elapsed
          l <- length
          pl <- played_long_enough ((-) <$> prev_elapsed <*> elapsed) length
          return (el < 60 && pr_el > el && pl)
        newElapsed :: Maybe Double
        newElapsed = case MPD.stTime newStatus of
          Just (elapsed, _) -> Just elapsed
          _ -> Nothing
        newLength :: Maybe MPD.Seconds
        newLength = case MPD.stTime newStatus of
          Just (_, length) -> Just length
          _ -> Nothing
        oldLength :: Maybe MPD.Seconds
        oldLength = case oldStatus >>= MPD.stTime of
          Just (_, length) -> Just length
          _ -> Nothing
        oldElapsed :: Maybe Double
        oldElapsed = case oldStatus >>= MPD.stTime of
          Just (elapsed, _) -> Just elapsed
          _ -> Nothing
        newSongID = MPD.stSongID newStatus
        oldSongID = oldStatus >>= MPD.stSongID
        sameSong :: Bool
        sameSong = let v = (==) <$> newSongID <*> oldSongID in isJust v && fromJust v
        laterPosition :: Bool
        laterPosition = let v = (>=) <$> newElapsed <*> oldElapsed in isJust v && fromJust v
        sameSongLaterPosition = sameSong && laterPosition


songToListen :: MPD.Song -> IO (Either String Listen)
songToListen song = do
  time <- fromEnum <$> epochTime
  return (makeListen >>= \l -> Right ((l albumID) { listened_at = Just time}))
  where getTag :: Metadata -> Maybe [MPD.Value]
        getTag t = sgGetTag t song
        albumID :: Maybe UUID
        albumID = case getUUIDTag MUSICBRAINZ_ALBUMID of
                    Left _ -> Nothing
                    Right u -> Just u
        getStringTag :: Metadata -> Either String String
        getStringTag t = case getTag t of
               Nothing -> Left (show t)
               Just v -> case toString <$> headMay v of
                           Just s -> Right s
                           Nothing -> Left (show t)
        getUUIDTag :: Metadata -> Either String UUID
        getUUIDTag t = case getTag t of
               Nothing -> Left (show t)
               Just v -> case toString <$> headMay v >>= fromString of
                           Just u -> Right u
                           Nothing -> Left (show t)
        makeListen :: Either String (Maybe UUID -> Listen)
        makeListen =
          Listen (Just 0)
          <$> getStringTag Title
          <*> getStringTag Artist
          <*> getUUIDTag MUSICBRAINZ_ARTISTID
          <*> getUUIDTag MUSICBRAINZ_TRACKID

scrobble :: Maybe MPD.Song -> Maybe MPD.Status -> MPD.Status -> IO ()
scrobble previousSong oldStatus newStatus = do
  if isJust previousSong && isListenWorthy oldStatus newStatus
    then doScrobble previousSong
    else print "No scrobbling necessary"
  idleHandle (Just newStatus)
  where doScrobble :: Maybe MPD.Song -> IO ()
        doScrobble ms = case ms of
          Nothing -> print "No song is playing"
          Just s -> songToListen s >>= submit
        submit :: Either String Listen -> IO ()
        submit = either print (print . encode)

idleHandle :: Maybe MPD.Status -> IO ()
idleHandle oldStatus = do
  resp <- getCurrentSong
  let currentsong = case resp of
        Right (Just song) -> Just song
        _ -> Nothing
    in idleMPD >>= \r -> handleResponse r currentsong oldStatus

handleResponse :: MPD.Response [Subsystem] -> Maybe MPD.Song -> Maybe MPD.Status -> IO ()
handleResponse resp previousSong previousStatus =
  either print (\_ -> getStatus >>= either print (scrobble previousSong previousStatus)) resp

main :: IO ()
main = do
  t <- token
  case t of
    Nothing -> die "LISTENBRAINZ_TOKEN is not set or is not a UUID"
    Just _ -> handleResponse (Right [PlayerS]) Nothing Nothing
