module Logs where

import Prelude

import Data.Bifunctor (lmap)
import Data.Traversable (traverse)
import Data.DateTime.Instant (Instant, instant)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Int64 as Int64
import ProtoHelper (class FromMessage, fromMessage, class SayError, toEither)
import System (Device, DeviceError)
import Proto.Logging.Logging (LOG_LEVEL(..), Log(..), Logs(..)) as Proto
import Proto.DeviceData.DeviceData (DEVICE) as Proto

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
  , device :: Device
  , content :: String
  }

data LogError
  = InvalidTimeStamp
  | MissingLogLevel
  | LogErrorLogLevel LogLevelError
  | MissingDevice
  | LogErrorDevice DeviceError
  | MissingContent

instance FromMessage Proto.Log Log LogError where
  fromMessage (Proto.Log msg) = do
    timestamp <- toEither InvalidTimeStamp $ do
      timestamp <- msg.timestampMs
      instant <<< Milliseconds $ Int64.toNumber timestamp

    levelProto <- toEither MissingLogLevel $ msg.level
    logLevel <- lmap LogErrorLogLevel $ fromMessage @Proto.LOG_LEVEL @LogLevel levelProto
    deviceProto <- toEither MissingDevice $ msg.device
    device <- lmap LogErrorDevice $ fromMessage @Proto.DEVICE @Device deviceProto
    content <- toEither MissingContent msg.content
    pure $ Log { timestamp, logLevel, device, content }


newtype Logs = Logs (Array Log)

data LogsError =
  MissingPartOfLogs (Array LogError)

instance FromMessage Proto.Logs Logs LogsError where
  fromMessage (Proto.Logs msg) = do
    foo <- lmap MissingPartOfLogs $
      ( traverse
          (\e -> lmap (\_ -> []) e)
          (fromMessage @Proto.Log @Log <$> msg.logs)
      )
    pure $ Logs foo
