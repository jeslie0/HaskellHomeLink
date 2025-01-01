module Radio (Format(..), RadioStream(..), Stream(..), radioStreams, StreamError(..)) where

import Prelude

import Data.Either (Either(..))
import Proto.Messages as Proto
import ProtoHelper (class FromMessage, class SayError, class ToMessage)

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

data StreamError = MissingStream
instance SayError StreamError where
  sayError MissingStream = pure "Missing stream"

derive instance streamEq :: Eq Stream

instance Show Stream where
  show ClassicFM = "Classic FM"
  show ClassicFMCalm = "Classic FM Calm"
  show ClassicFMMovies = "Classic FM Movies"
  show RadioXClassicRock = "Radio X Classic Rock"
  show LBC = "LBC"
  show Unknown = "unknown radio station"

instance FromMessage Proto.STREAM Stream StreamError where
  fromMessage Proto.STREAM_CLASSIC_FM = Right ClassicFM
  fromMessage Proto.STREAM_CLASSIC_FM_CALM = Right ClassicFMCalm
  fromMessage Proto.STREAM_CLASSIC_FM_MOVIES = Right ClassicFMMovies
  fromMessage Proto.STREAM_RADIO_X_CLASSIC_ROCK = Right RadioXClassicRock
  fromMessage Proto.STREAM_LBC = Right LBC
  fromMessage _ = Left MissingStream

instance ToMessage Proto.STREAM Stream where
  toMessage ClassicFM = Proto.STREAM_CLASSIC_FM
  toMessage ClassicFMCalm = Proto.STREAM_CLASSIC_FM_CALM
  toMessage ClassicFMMovies = Proto.STREAM_CLASSIC_FM_MOVIES
  toMessage RadioXClassicRock = Proto.STREAM_RADIO_X_CLASSIC_ROCK
  toMessage LBC = Proto.STREAM_LBC
  toMessage Unknown = Proto.STREAM_UNKNOWN_STREAM

type RadioStream = { stream :: Stream, url :: String, format :: Format }

radioStreams :: Array RadioStream
radioStreams =
  [ { stream: ClassicFM, url: "https://media-ice.musicradio.com/ClassicFMMP3", format: MP3 }
  , { stream: ClassicFMCalm, url: "https://media-ice.musicradio.com/ClassicFMCalmMP3", format: MP3 }
  , { stream: ClassicFMMovies, url: "https://media-ice.musicradio.com/ClassicFMMoviesMP3", format: MP3 }
  , { stream: RadioXClassicRock, url: "https://media-ice.musicradio.com/RadioXClassicRockMP3", format: MP3 }
  , { stream: LBC, url: "https://media-ice.musicradio.com/LBCUKMP3", format: MP3 }
  ]
