module Home.AudioStreamTypes where

import Proto.Radio qualified as Proto
import ProtoHelper (FromMessage (fromMessage), ToMessage (toMessage))
import Data.Word (Word32)

type StationId = Word32

data StreamStatus = Off | Initiated | Playing
  deriving (Eq)

instance FromMessage Proto.STREAM_STATUS StreamStatus where
  fromMessage Proto.OFF = Off
  fromMessage Proto.INITIATED = Initiated
  fromMessage Proto.PLAYING = Playing
  fromMessage _ = Off

instance ToMessage Proto.STREAM_STATUS StreamStatus where
  toMessage Off = Proto.OFF
  toMessage Initiated = Proto.INITIATED
  toMessage Playing = Proto.PLAYING
