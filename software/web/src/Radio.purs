module Radio (Format(..), RadioStream(..), Stream(..), radioStreams, StreamError(..), StreamStatus(..), StreamStatusError) where

import Prelude

import Data.Either (Either(..))
import Data.UInt (UInt, fromInt)
import Proto.Messages as Proto
import ProtoHelper (class FromMessage, class ToMessage)
import Protobuf.Internal.Prelude (toInt)

data Format
  = MP3
  | PCM

derive instance formatEq :: Eq Format

data Stream
  = ClassicFM
  | ClassicFMCalm
  | ClassicFMMovies
  | RadioXClassicRock
  | LBC
  | Unknown

derive instance streamEq :: Eq Stream

instance Show Stream where
  show ClassicFM = "Classic FM"
  show ClassicFMCalm = "Classic FM Calm"
  show ClassicFMMovies = "Classic FM Movies"
  show RadioXClassicRock = "Radio X Classic Rock"
  show LBC = "LBC"
  show Unknown = "unknown radio station"

data StreamError

instance FromMessage UInt Stream StreamError where
  fromMessage n = case toInt n of
    0 -> Right ClassicFM
    1 -> Right ClassicFMCalm
    2 -> Right ClassicFMMovies
    3 -> Right RadioXClassicRock
    4 -> Right LBC
    _ -> Right Unknown

instance ToMessage UInt Stream where
  toMessage ClassicFM = fromInt 0
  toMessage ClassicFMCalm = fromInt 1
  toMessage ClassicFMMovies = fromInt 2
  toMessage RadioXClassicRock = fromInt 3
  toMessage LBC = fromInt 4
  toMessage Unknown = fromInt 5

data StreamStatus
  = Off
  | Initiated
  | Playing

instance Show StreamStatus where
  show Off = "Off"
  show Initiated = "Initiated"
  show Playing = "Playing"

derive instance streamStatusEq :: Eq StreamStatus

data StreamStatusError

instance FromMessage Proto.STREAM_STATUS StreamStatus StreamStatusError where
  fromMessage Proto.STREAM_STATUS_INITIATED = Right Initiated
  fromMessage Proto.STREAM_STATUS_PLAYING = Right Playing
  fromMessage _ = Right Off

type RadioStream = { stream :: Stream, url :: String, format :: Format }

radioStreams :: Array RadioStream
radioStreams =
  [ { stream: ClassicFM, url: "https://media-ice.musicradio.com/ClassicFMMP3", format: MP3 }
  , { stream: ClassicFMCalm, url: "https://media-ice.musicradio.com/ClassicFMCalmMP3", format: MP3 }
  , { stream: ClassicFMMovies, url: "https://media-ice.musicradio.com/ClassicFMMoviesMP3", format: MP3 }
  , { stream: RadioXClassicRock, url: "https://media-ice.musicradio.com/RadioXClassicRockMP3", format: MP3 }
  , { stream: LBC, url: "https://media-ice.musicradio.com/LBCUKMP3", format: MP3 }
  ]
