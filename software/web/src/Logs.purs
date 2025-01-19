module Logs where

import Prelude

import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, instant)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.UInt as UInt
import Proto.Messages as Proto
import ProtoHelper (class FromMessage, fromMessage, class SayError, toEither)
import System (Island, IslandError)

data LogLevel
  = Undefined
  | Trace
  | Debug
  | Info
  | Warn
  | Error
  | Fatal

instance Show LogLevel where
  show Undefined = "Undefined"
  show Trace = "Trace"
  show Debug = "Debug"
  show Info = "Info"
  show Warn = "Warn"
  show Error = "Error"
  show Fatal = "Fatal"

data LogLevelError = LogLevelIsUndefined

instance SayError LogLevelError where
  sayError LogLevelIsUndefined = pure "Error: Log level is undefined"

instance FromMessage Proto.LOG_LEVEL LogLevel LogLevelError where
  fromMessage (Proto.LOG_LEVEL_UNDEFINED) = Left LogLevelIsUndefined
  fromMessage (Proto.LOG_LEVEL_TRACE) = Right Trace
  fromMessage (Proto.LOG_LEVEL_DEBUG) = Right Debug
  fromMessage (Proto.LOG_LEVEL_INFO) = Right Info
  fromMessage (Proto.LOG_LEVEL_WARN) = Right Warn
  fromMessage (Proto.LOG_LEVEL_ERROR) = Right Error
  fromMessage (Proto.LOG_LEVEL_FATAL) = Right Fatal

data Log = Log
  { timestamp :: Instant
  , logLevel :: LogLevel
  , island :: Island
  , messages :: Array String
  }

data LogError
  = InvalidTimeStamp
  | MissingLogLevel
  | LogErrorLogLevel LogLevelError
  | MissingIsland
  | LogErrorIsland IslandError

instance FromMessage Proto.Log Log LogError where
  fromMessage (Proto.Log msg) = do
    timestamp <- toEither InvalidTimeStamp $ do
      timestamp <- msg.timestampMs
      instant <<< Milliseconds $ UInt.toNumber timestamp

    levelProto <- toEither MissingLogLevel $ msg.level
    logLevel <- lmap LogErrorLogLevel $ fromMessage @Proto.LOG_LEVEL @LogLevel levelProto
    islandProto <- toEither MissingIsland $ msg.island
    island <- lmap LogErrorIsland $ fromMessage @Proto.ISLAND @Island islandProto
    let messages = msg.messages
    pure $ Log { timestamp, logLevel, island, messages }
