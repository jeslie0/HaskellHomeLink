module Requests
  ( fetchStreamStatus
  , modifyStream
  , fetchSystemsData
  , fetchMemoryData
  ) where

import Prelude

import Apexcharts (Apexchart, updateOptions, render)
import Chart (example, newOptions)
import Constants (getApiUrl)
import Data.Array as Array
import Data.ArrayBuffer.Builder (execPutM)
import Data.ArrayBuffer.DataView (byteLength, whole)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Fetch (fetch)
import Parsing (fail, runParserT)
import Proto.Messages as Proto
import ProtoHelper (fromMessage, sayError, toMessage)
import Radio (Stream(..), StreamStatus(..), StreamStatusError, radioStreams)
import System (AllIslandsMemoryData(..), Island(..), IslandMemoryInformation(..), IslandsSystemData, MemoryInformation(..))

-- * Stream requests

-- | Get the current stream state and update appropriately.
fetchStreamStatus
  :: Ref.Ref Int
  -> (Stream -> Effect Unit)
  -> (StreamStatus -> Effect Unit)
  -> Effect Unit
fetchStreamStatus streamStateIdRef updateStreamStation updateStreamStatus = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio"
  launchAff_ do
    { arrayBuffer } <- fetch requestUrl { method: GET }
    body <- whole <$> arrayBuffer
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseGetRadioStatusResponse (byteLength body)
      parseResponse resp
    liftEffect $ actOnResult result
    pure unit

  where
  setStreamStateId n = Ref.write n streamStateIdRef

  parseResponse (Proto.GetRadioStatusResponse { stateId: mStateId, status: mStatus, currentStationId: mStationId }) =
    pure $ fromMaybe 0 mStateId
      /\ maybe (Right Off) (fromMessage @_ @_ @StreamStatusError) mStatus
      /\ maybe (Right ClassicFM) (fromMessage) mStationId

  actOnResult (Left _) = Console.log "FAIL"
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
              Console.logShow status
              updateStreamStation station

-- | Start or stop the audio stream.
modifyStream
  :: Ref.Ref Int
  -> (Stream -> Effect Unit)
  -> (StreamStatus -> Effect Unit)
  -> (Maybe Stream)
  -> Effect Unit
modifyStream stateIdRef updateStation updateStreamStatus payload = do
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
    liftEffect $ fetchStreamStatus stateIdRef updateStation updateStreamStatus

-- | Get the current system data
fetchSystemsData :: (IslandsSystemData -> Effect Unit) -> Effect Unit
fetchSystemsData update = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "system"
  launchAff_ do
    { arrayBuffer } <- fetch requestUrl { method: GET }
    body <- whole <$> arrayBuffer
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseIslandsSystemData (byteLength body)
      case fromMessage resp of
        Left errs -> fail <<< show $ sayError errs
        Right islandsData -> pure islandsData
    case result of
      Left err -> liftEffect $ Console.logShow err
      Right islandsData -> do
        liftEffect $ update islandsData
    pure unit

fetchMemoryData
  :: (Array Island -> Effect Unit)
  -> Ref.Ref (Array (Tuple Island Apexchart))
  -> Effect Unit
fetchMemoryData setExistingApexCharts apexRef = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "memory"
  launchAff_ do
    { arrayBuffer } <- fetch requestUrl { method: GET }
    body <- whole <$> arrayBuffer
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseAllIslandMemoryData (byteLength body)
      case fromMessage resp of
        Left errs -> fail "Error parsing response"
        Right allIslandsMemoryData -> pure allIslandsMemoryData
    case result of
      Left err -> liftEffect $ Console.log "Error getting memory data"
      Right allIslandsMemoryData -> do
        liftEffect $ actOnData apexRef allIslandsMemoryData
    pure unit

  where
  actOnData :: Ref.Ref (Array (Tuple Island Apexchart)) -> AllIslandsMemoryData -> Effect Unit
  actOnData chartArrayRef (AllIslandsMemoryData { allIslandMemoryData }) =
    for_ [ IslandMemoryInformation { island: Home, memInfo: (MemoryInformation { systemTime: fromInt 0, memUsed: fromInt 0 }) } ] $ \(IslandMemoryInformation { island, memInfo }) -> do
      chartArray <- liftEffect $ Ref.read apexRef
      case Array.find ((==) island <<< fst) chartArray of
        Nothing -> do
          setExistingApexCharts $ Array.sort (Array.cons Home $ map fst chartArray)
          void $ setTimeout 1000 $ do
            newIslandChart <- liftEffect $ example ("chart")
            render newIslandChart
            let
                newActiveCharts =
                  Array.sortBy
                    (\a b -> if fst a < fst b then LT else if fst a == fst b then EQ else GT) $
                    Array.cons (Tuple island newIslandChart) chartArray
            Ref.write newActiveCharts chartArrayRef

        Just (Tuple _ chart) -> do
          -- TODO use chart data here to make plot
          void $ setTimeout 2000 $ updateOptions newOptions chart
          pure unit
