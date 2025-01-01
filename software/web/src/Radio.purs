module Radio (Format(..), RadioStream(..), Stream(..), radioStreams) where

import Prelude

data Format
  = MP3
  | PCM

derive instance formatEq :: Eq Format

data Stream =
 ClassicFM
 | ClassicFMCalm
 | ClassicFMMovies
 | RadioXClassicRock
 | LBC

derive instance streamEq :: Eq Stream

instance Show Stream where
  show ClassicFM = "Classic FM"
  show ClassicFMCalm = "Classic FM Calm"
  show ClassicFMMovies = "Classic FM Movies"
  show RadioXClassicRock = "Radio X Classic Rock"
  show LBC = "LBC"

type RadioStream = { stream :: Stream, url :: String, format :: Format }

radioStreams :: Array RadioStream
radioStreams = [ { stream: ClassicFM, url: "https://media-ice.musicradio.com/ClassicFMMP3", format: MP3 }
               , { stream: ClassicFMCalm, url: "https://media-ice.musicradio.com/ClassicFMCalmMP3", format: MP3 }
               , { stream: ClassicFMMovies, url: "https://media-ice.musicradio.com/ClassicFMMoviesMP3", format: MP3 }
               , { stream: RadioXClassicRock, url: "https://media-ice.musicradio.com/RadioXClassicRockMP3", format: MP3 }
               , { stream: LBC, url: "https://media-ice.musicradio.com/LBCUKMP3", format: MP3 }]
