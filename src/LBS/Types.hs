{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module LBS.Types where

import           Control.Monad      (liftM)
import           Data.Aeson         (ToJSON (..), object, (.=))
import           Data.Text          (pack)
import           Data.UUID          (UUID, fromString)
import           Data.UUID.Aeson    ()
import qualified Network.MPD        as MPD
import           System.Clock       (TimeSpec)
import           System.Environment (lookupEnv)

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
  , releaseName :: Maybe String
  , releaseID   :: Maybe UUID
  }

instance ToJSON Listen where
  toJSON (Listen {..}) = object
                         [ "listened_at" .= listenedAt
                         , "track_metadata" .= object
                           [ "artist_name" .= pack artistName
                           , "track_name" .= pack trackName
                           , "release_name" .= releaseName
                           , "additional_info" .= object
                             [ "recording_mbid" .= recordingID
                             , "artist_mbids" .= artistID
                             , "release_mbid" .= releaseID
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
