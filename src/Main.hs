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
import           System.Clock          (Clock (Monotonic), TimeSpec,
                                        diffTimeSpec, getTime,
                                        timeSpecAsNanoSecs)
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
  { playerStatus             :: MPD.Status
  , song                     :: Maybe MPD.Song
  , lastClock                :: TimeSpec
  , timeElapsedInCurrentSong :: Maybe TimeSpec
  }

type Nanoseconds = Integer

secondsToNanoseconds :: MPD.Seconds -> Nanoseconds
secondsToNanoseconds = (*10^9)

idleMPD :: IO (MPD.Response [Subsystem])
idleMPD = withMPD (idle [PlayerS])

getStatus :: IO (MPD.Response MPD.Status)
getStatus = withMPD MPD.status

getCurrentSong :: IO (MPD.Response (Maybe MPD.Song))
getCurrentSong = withMPD MPD.currentSong

isListenWorthy :: Maybe State -> MPD.Status -> Bool
isListenWorthy oldState newStatus =
  if stateKeptPlaying then
    case oldStatus of
      -- If we don't know the previous state, we're most likely at the
      -- startup phase, so just assume the song is not listen worthy
      Nothing -> False
      Just _ -> let pl = played_long_enough (fromJust oldElapsed) (fromJust oldLength) -- Check if we played the old song long enough to make it scrobbleworthy
                    -- rep = playing_repeatedly oldElapsed newElapsed newLength
                in pl
  else False
  where newState = MPD.stState newStatus
        oldStatus = playerStatus <$> oldState
        oldPlayerState = MPD.stState <$> oldStatus
        stateKeptPlaying = (newState == MPD.Playing) && (oldPlayerState == Just MPD.Playing)
        played_long_enough :: TimeSpec -> MPD.Seconds -> Bool
        played_long_enough elapsed length =
          let el = fromIntegral (timeSpecAsNanoSecs elapsed)
              l = secondsToNanoseconds length
              in (el > minPlayTime || l >= minLength && (fromIntegral el) > fromIntegral l / 2)
        minPlayTime :: Nanoseconds
        minPlayTime = secondsToNanoseconds 240
        minLength :: Nanoseconds
        minLength = secondsToNanoseconds 30
        -- Check if the song is playing on repeat. This doesn't work
        -- yet because prev_elapsed is from the beginning of the
        -- previous song and thus too small.
        -- TODO set up time measurement like mpdscribble does
        playing_repeatedly :: Maybe Double -> Maybe Double -> Maybe MPD.Seconds -> Maybe Bool
        playing_repeatedly prev_elapsed elapsed length = Just False
          -- pr_el <- prev_elapsed
          -- el <- elapsed
          -- l <- length
          -- pl <- played_long_enough ((-) <$> prev_elapsed <*> elapsed) length
          -- return (el < 60 && pr_el > el && pl)
        newLength :: Maybe MPD.Seconds
        newLength = case MPD.stTime newStatus of
          Just (_, length) -> Just length
          _ -> Nothing
        oldLength :: Maybe MPD.Seconds
        oldLength = case oldStatus >>= MPD.stTime of
          Just (_, length) -> Just length
          _ -> Nothing
        oldElapsed :: Maybe TimeSpec
        oldElapsed = oldState >>= timeElapsedInCurrentSong

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
  currentSongResp <- getCurrentSong
  currentClockTick <- getTime Monotonic
  let currentSong = case currentSongResp of
               Right s -> s
               _ -> Nothing
      lastClockTick = lastClock <$> previousState
      newPlayerState = MPD.stState newStatus
      previousPlayerState = MPD.stState <$> playerStatus <$> previousState
      currentlyElapsed = previousState >>= timeElapsedInCurrentSong
      newElapsed = calculateElapsed previousPlayerState newPlayerState lastClockTick currentClockTick currentlyElapsed
      newState = previousState >>= (\s -> Just (s {timeElapsedInCurrentSong = newElapsed}))
  -- Perform the scrobbling, if necessary
  if isJust previousState && isListenWorthy newState newStatus
    then doScrobble (newState >>= song)
    else print "No scrobbling necessary"

  -- Wait for the next event of the player subsystem with an updated state
  idleHandle (Just (State newStatus currentSong currentClockTick newElapsed))
  where doScrobble :: Maybe MPD.Song -> IO ()
        doScrobble ms = case ms of
          Nothing -> print "No song is playing"
          Just s -> songToListen s >>= either print submit
        calculateElapsed :: Maybe MPD.State -> MPD.State -> Maybe TimeSpec -> TimeSpec -> Maybe TimeSpec -> Maybe TimeSpec
        -- 1. If the state changed from playing to pause, return the
        -- time between the last clock tick and now
        calculateElapsed (Just MPD.Playing) MPD.Paused (Just lastClockTick) currentClockTick _ = Just (diffTimeSpec currentClockTick lastClockTick)
        -- 2. If the state changed from pause back to playing, do
        -- nothing with the value
        calculateElapsed (Just MPD.Paused) MPD.Playing _ _ currentlyElapsed = currentlyElapsed
        -- 3. A state change from playing to playing. This means that
        -- the song changed and the new elapsed value should be the
        -- sum of the last elapsed value and the time between the last clock tick and the current one
        calculateElapsed (Just MPD.Playing) MPD.Playing (Just lastClockTick) currentClockTick (Just currentlyElapsed) = Just (currentlyElapsed + diffTimeSpec currentClockTick lastClockTick)
        -- 4. Any other state change means the stopped state is
        -- involved or the song changed, so set the value to 0 as well
        calculateElapsed _ _ currentClockTick _ _ = diffTimeSpec <$> currentClockTick <*> currentClockTick


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
