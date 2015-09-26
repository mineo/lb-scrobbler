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
import           Network.HTTP.Client   (HttpException, defaultManagerSettings,
                                        managerResponseTimeout)
import           Network.MPD           (Metadata (..), Subsystem (..), idle,
                                        sgGetTag, toString, withMPD)
import qualified Network.MPD           as MPD
import           Network.Wreq          (defaults, header, manager, postWith)
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

data Listen = Listen
  { listenedAt  :: Maybe Int
  , trackName   :: String
  , artistName  :: String
  , artistID    :: [UUID]
  , recordingID :: UUID
  , releaseID   :: Maybe UUID
  }

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

data State = State
  { playerStatus   :: MPD.Status
  , song           :: Maybe MPD.Song
  , alreadyElapsed :: Maybe Double
  }

idleMPD :: IO (MPD.Response [Subsystem])
idleMPD = withMPD (idle [PlayerS])

getStatus :: IO (MPD.Response MPD.Status)
getStatus = withMPD MPD.status

getCurrentSong :: IO (MPD.Response (Maybe MPD.Song))
getCurrentSong = withMPD MPD.currentSong

isListenWorthy :: Maybe State -> MPD.Status -> Bool
isListenWorthy oldState newStatus =
  if stateKeptPlaying&& not sameSongLaterPosition then
    case oldStatus of
      Nothing -> let pl = played_long_enough newElapsed newLength in newState == MPD.Playing && isJust pl && fromJust pl
      Just _ -> let pl = played_long_enough oldElapsed oldLength -- Check if we played the old song long enough to make it scrobbleworthy
                    rep = playing_repeatedly oldElapsed newElapsed newLength
                    val = (||) <$> pl <*> rep
                in isJust val && fromJust val
  else False
  where newState = MPD.stState newStatus
        oldStatus = playerStatus <$> oldState
        oldPlayerState = MPD.stState <$> oldStatus
        stateKeptPlaying = (newState == MPD.Playing) && (oldPlayerState == Just MPD.Playing)
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

extractElapsed :: MPD.Status -> Maybe Double
extractElapsed s = case MPD.stTime s of
  Just (elapsed, _) -> Just elapsed
  _ -> Nothing

extractLength :: MPD.Status -> Maybe MPD.Seconds
extractLength s = case MPD.stTime s of
  Just (_, length) -> Just length
  _ -> Nothing

songToListen :: MPD.Song -> IO (Either String Listen)
songToListen song = do
  time <- fromEnum <$> epochTime
  return (makeListen >>= \l -> Right ((l albumID) { listenedAt = Just time}))
  where getTag :: Metadata -> Maybe [MPD.Value]
        getTag t = sgGetTag t song
        getTagCase :: Metadata -> ([MPD.Value] -> Either String t) -> Either String t
        getTagCase t f = case getTag t of
          Nothing -> Left (show t)
          Just u -> case f u of
            Right c -> Right c
            Left s -> Left s
        albumID :: Maybe UUID
        albumID = case getUUIDTag MUSICBRAINZ_ALBUMID of
                    Left _ -> Nothing
                    Right u -> Just u
        getStringTag :: Metadata -> Either String String
        getStringTag t = getTagCase t
                         (\v -> case toString <$> headMay v of
                           Just s -> Right s
                           Nothing -> Left (show t))
        getUUIDTag :: Metadata -> Either String UUID
        getUUIDTag t = getTagCase t
                       (\v -> case toString <$> headMay v >>= fromString of
                           Just u -> Right u
                           Nothing -> Left (show t))
        getUUIDList :: Metadata -> Either String [UUID]
        getUUIDList t = getTagCase t
                        (\v -> case fromString <$> toString <$> (v::[MPD.Value]) of
                           [] -> Left (show t)
                           -- If all strings could be converted to
                           -- UUIDs, return the list of UUIDs, if one
                           -- of them couldn't, return Left
                           l -> if foldl (\o n -> o && isJust n) True l
                                then Right (fromJust <$> l)
                                else Left (show t))
        makeListen :: Either String (Maybe UUID -> Listen)
        makeListen =
          Listen (Just 0)
          <$> getStringTag Title
          <*> getStringTag Artist
          <*> getUUIDList MUSICBRAINZ_ARTISTID
          <*> getUUIDTag MUSICBRAINZ_TRACKID

scrobble :: Maybe State -> MPD.Status -> IO ()
scrobble previousState newStatus = do
  -- Perform the scrobbling, if necessary
  if isJust previousState && isListenWorthy previousState newStatus
    then doScrobble (previousState >>= song)
    else print "No scrobbling necessary"

  -- Wait for the next event of the player subsystem with an updated state
  currentSongResp <- getCurrentSong
  let song = case currentSongResp of
               Right s -> s
               _ -> Nothing
      already = previousState >>= alreadyElapsed
      newState = MPD.stState newStatus
      previousPlayerState = MPD.stState <$> playerStatus <$> previousState
      currentlyElapsed = extractElapsed newStatus
      newElapsed = calculateElapsed previousPlayerState newState already currentlyElapsed
  idleHandle (Just (State newStatus song newElapsed))
  where doScrobble :: Maybe MPD.Song -> IO ()
        doScrobble ms = case ms of
          Nothing -> print "No song is playing"
          Just s -> songToListen s >>= either print submit
        calculateElapsed :: Maybe MPD.State -> MPD.State -> Maybe Double -> Maybe Double -> Maybe Double
        -- 1. If the state changed from playing to pause, save the
        -- amount of seconds elapsed so far
        calculateElapsed (Just MPD.Playing) MPD.Paused _ currently = currently
        -- 2. If the state changed from pause back to playing, do
        -- nothing with the value
        calculateElapsed (Just MPD.Paused) MPD.Playing already _ = already
        -- 3. If the state changed from playing to playing, most
        -- likely the song changed → set the value back to 0
        calculateElapsed (Just MPD.Playing) MPD.Playing _ _ = Just 0
        -- 4. If any other state change means the stopped state is
        -- involved, so set the value to 0 as well
        calculateElapsed _ _ _ _ = Just 0

submit :: Listen -> IO ()
submit listen =
  handle handleHTTPException (upload (Single listen))
  where upload :: Request -> IO ()
        upload request = do
          t <- token
          _ <- postWith (( set
                          (header "Authorization")
                          [BS.pack ("Token " ++ UUID.toString ( fromJust t))]
                        . set
                          manager
                          (Left (defaultManagerSettings { managerResponseTimeout = Just 20000000})))
                         defaults)
               "http://listenbrainz.org/1/submit-listens"
               (toJSON request)
          print "Upload successful"
        handleHTTPException :: HttpException -> IO ()
        handleHTTPException = print . show

idleHandle :: Maybe State -> IO ()
idleHandle oldState = idleMPD >>= \r -> handleResponse r oldState

handleResponse :: MPD.Response [Subsystem] -> Maybe State -> IO ()
handleResponse resp state = either print (\_ -> getStatus >>= either print (scrobble state)) resp

checkEnv :: IO (Maybe a) -> String -> IO ()
checkEnv var name = do
  t <- var
  case t of
    Nothing -> die (name ++ "is not set correctly")
    Just _ -> return ()

main :: IO ()
main = do
  checkEnv token "LISTENBRAINZ_TOKEN"
  handleResponse (Right [PlayerS]) Nothing
