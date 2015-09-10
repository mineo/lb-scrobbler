{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Exception     (handle)
import           Control.Lens.Setter   (set)
import           Control.Monad         (liftM)
import           Data.Aeson            (ToJSON (..), object, (.=))
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromJust, isJust)
import           Data.Text             (pack)
import           Data.UUID             (UUID, fromString)
import qualified Data.UUID             as UUID
import           Data.UUID.Aeson       ()
import           GHC.Generics
import           Network.HTTP.Client   (HttpException)
import           Network.MPD           (Metadata (..), Subsystem (..), idle,
                                        sgGetTag, toString, withMPD)
import qualified Network.MPD           as MPD
import           Network.Wreq          (defaults, header, postWith)
import           Safe                  (headMay)
import           System.Environment    (lookupEnv)
import           System.Exit           (die)
import           System.Posix.Time     (epochTime)

type Token = UUID
type User = String

token :: IO (Maybe Token)
token =
  liftM (\l -> case l of
    Nothing -> Nothing
    Just t -> fromString t)
  (lookupEnv "LISTENBRAINZ_TOKEN")

user :: IO (Maybe User)
user =
  liftM (\l -> case l of
    Nothing -> Nothing
    Just _ -> l)
  (lookupEnv "LISTENBRAINZ_USER")


data Listen = Listen
  { listenedAt  :: Maybe Int
  , trackName   :: String
  , artistName  :: String
  , artistID    :: UUID
  , recordingID :: UUID
  , releaseID   :: Maybe UUID
  } deriving (Generic)

instance ToJSON Listen where
  toJSON (Listen {..}) = object
                         [ "listened_at" .= listenedAt
                         , "track_metadata" .= object
                           [ "artist_name" .= pack artistName
                           , "track_name" .= pack trackName
                           , "additional_info" .= object
                             [ "recording_id" .= recordingID
                             , "artist_id" .= artistID
                             , "release_id" .= releaseID
                             , "tags" .= ([]::[Bool])
                             ]
                           ]
                         ]

data Request = Single Listen -- TODO: Add Import & PlayingNow

instance ToJSON Request where
  toJSON (Single l) = object [ "listen_type" .= ("single"::String)
                             , "payload" .= [l]
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
  return (makeListen >>= \l -> Right ((l albumID) { listenedAt = Just time}))
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
submit listen = case listen of
  Left s -> print s
  Right l -> handle handleHTTPException (upload (Single l))
  where upload :: Request -> IO ()
        upload request = do
          u <- user
          t <- token
          _ <- postWith (set
                         (header "Authorization")
                         [BS.pack ("Token " ++ UUID.toString ( fromJust t))]
                         defaults)
               ("http://listenbrainz.org/listen/user/" ++ (fromJust u))
               (toJSON request)
          print "Upload successful"
        handleHTTPException :: HttpException -> IO ()
        handleHTTPException = print . show

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

checkEnv :: IO (Maybe a) -> String -> IO ()
checkEnv var name = do
  t <- var
  case t of
    Nothing -> die (name ++ "is not set correctly")
    Just _ -> return ()

main :: IO ()
main = do
  checkEnv token "LISTENBRAINZ_TOKEN"
  checkEnv user "LISTENBRAINZ_USER"
  handleResponse (Right [PlayerS]) Nothing Nothing
