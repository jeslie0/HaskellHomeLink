module Requests
  ( AppPollers
  , mkStreamStatusPoller
  , modifyStream
  , mkSystemsDataPoller
  , mkMemoryChartOptionsPoller
  , mkLogsPoller
  ) where

import Prelude

import Apexcharts (Apexoptions)
import Chart (defaultChartOptions, updatedChartOptions)
import Constants (getApiUrl)
import Data.Array as Array
import Data.ArrayBuffer.Builder (execPutM)
import Data.ArrayBuffer.DataView (byteLength)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Options (Options)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Fetch (fetch)
import Logs (Log, Logs(..))
import Parsing (fail, runParserT)
import Poller (Poller, mkPoller)
import Proto.DeviceData.DeviceData (parseAllDeviceData, parseAllDeviceMemoryData) as Proto
import Proto.Logging.Logging (parseLogs) as Proto
import Proto.Radio.Radio (GetRadioStatusResponse(..), RadioStation(..), defaultRadioStation, mkModifyRadioRequest, parseGetRadioStatusResponse, putModifyRadioRequest) as Proto
import ProtoHelper (fromMessage, sayError, toMessage)
import Radio (Stream(..), StreamStatus(..), StreamStatusError, radioStreams)
import System (AllDeviceData(..), AllDevicesMemoryData(..), Device(..), DeviceData, DeviceDataPair(..), DeviceMemoryInformation(..))

type AppPollers =
  { streamStatusPoller :: Poller
  , memoryDataPoller :: Poller
  , systemsDataPoller :: Poller
  , logsPoller :: Poller
  }

-- * Stream requests

-- | Get the current stream state and update appropriately.
mkStreamStatusPoller
  :: Ref.Ref Int
  -> (Stream -> Effect Unit)
  -> (StreamStatus -> Effect Unit)
  -> Effect Poller
mkStreamStatusPoller streamStateIdRef updateStreamStation updateStreamStatus = do
  mkPoller "radio" 2000 $ \body -> do
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseGetRadioStatusResponse (byteLength body)
      parseResponse resp
    liftEffect $ actOnResult result

  where
  setStreamStateId n =
    Ref.write n streamStateIdRef

  parseResponse (Proto.GetRadioStatusResponse { stateId: mStateId, status: mStatus, currentStationId: mStationId }) =
    pure $ fromMaybe 0 mStateId
      /\ maybe (Right Off) (fromMessage @_ @_ @StreamStatusError) mStatus
      /\ maybe (Right ClassicFM) (fromMessage) mStationId

  actOnResult (Left _) =
    Console.log "FAIL"

  actOnResult (Right (stateId /\ eStatus /\ eCurrentStationId)) = do
    currStateId <- Ref.read streamStateIdRef
    if currStateId == stateId then pure unit
    else do
      setStreamStateId stateId
      case eStatus of
        Left _ -> Console.log "Couldn't parse status"
        Right status -> do
          case eCurrentStationId of
            Left _ -> Console.log "Couldn't parse currentStationId"
            Right station -> do
              updateStreamStatus status
              updateStreamStation station

-- | Start or stop the audio stream.
modifyStream
  :: Ref.Ref Int
  -> Poller
  -> Maybe Stream
  -> Effect Unit
modifyStream stateIdRef ({ force }) payload = do
  id <- Ref.read stateIdRef
  apiUrl <- getApiUrl
  let
    requestUrl = apiUrl <> "radio/modify" <> "?stateId=" <> show id
    body = Proto.mkModifyRadioRequest
      { station: payload <#> \stream ->
          Proto.RadioStation $ Proto.defaultRadioStation
            { url = Array.find (\rstream -> rstream.stream == stream) radioStreams <#> _.url
            , id = Just $ toMessage stream
            }
      }
  bodyBuff <- execPutM $ Proto.putModifyRadioRequest body
  launchAff_ do
    _ <- fetch requestUrl
      { method: POST
      , body: bodyBuff
      , headers: { "content-type": "application/protobuf" }
      }
    pure unit
    liftEffect force

-- | Get the current system data
mkSystemsDataPoller
  :: { setHomeDeviceDataPoll :: Maybe DeviceData -> Effect Unit
     , setProxyDeviceDataPoll :: Maybe DeviceData -> Effect Unit
     }
  -> Effect Poller
mkSystemsDataPoller { setHomeDeviceDataPoll, setProxyDeviceDataPoll } = do
  mkPoller "system" 5000 $ \body -> do
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseAllDeviceData (byteLength body)
      case fromMessage resp of
        Left errs -> fail <<< show $ sayError errs
        Right devicesData -> do
          pure devicesData
    case result of
      Left err -> liftEffect $ Console.logShow err
      Right (AllDeviceData { allDeviceData }) -> do
        let
          updateDevice device callback =
            callback $ (\(DeviceDataPair msg) -> msg.deviceData) <$> Array.find (\(DeviceDataPair msg) -> msg.device == device) allDeviceData
        liftEffect $ updateDevice Home setHomeDeviceDataPoll
        liftEffect $ updateDevice Proxy setProxyDeviceDataPoll

mkMemoryChartOptionsPoller
  :: { setHomeMemoryChartOptionsPoll :: Options Apexoptions -> Effect Unit
     , setProxyMemoryChartOptionsPoll :: Options Apexoptions -> Effect Unit
     }
  -> Effect Poller
mkMemoryChartOptionsPoller { setHomeMemoryChartOptionsPoll, setProxyMemoryChartOptionsPoll } = do
  mkPoller "memory" (30 * 1000) $ \body -> do
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseAllDeviceMemoryData (byteLength body)
      case fromMessage resp of
        Left _errs -> fail "Error parsing response"
        Right allDevicesMemoryData -> do
          pure allDevicesMemoryData
    case result of
      Left err ->
        liftEffect $ Console.log $ "Error getting memory data: " <> show err
      Right (AllDevicesMemoryData { allDeviceMemoryData }) -> do
        let
          getOptions :: Device -> (Options Apexoptions)
          getOptions device =
            fromMaybe (defaultChartOptions 10.0) $ do
              DeviceMemoryInformation info <-
                Array.find
                  (\(DeviceMemoryInformation info) -> info.device == device)
                  allDeviceMemoryData
              pure $ updatedChartOptions info.timeMem
        liftEffect <<< setHomeMemoryChartOptionsPoll <<< getOptions $ Home
        liftEffect <<< setProxyMemoryChartOptionsPoll <<< getOptions $ Proxy

mkLogsPoller :: (Array Log -> Effect Unit) -> Effect Poller
mkLogsPoller setLogsPoll = do
  mkPoller "logs" (5 * 1000) $ \body -> do
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseLogs (byteLength body)
      case fromMessage resp of
        Left _errs -> fail "Error parsing response"
        Right (Logs logs) -> pure logs
    case result of
      Left err -> liftEffect $ Console.log $ "Error getting logs: " <> show err
      Right logs -> do
        liftEffect $ setLogsPoll logs
    pure unit
