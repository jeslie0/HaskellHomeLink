module Requests
  ( fetchStreamStatus
  , modifyStream
  , fetchSystemsData
  ) where

import Prelude

import Constants (getApiUrl)
import Data.ArrayBuffer.Builder (execPutM)
import Data.ArrayBuffer.DataView (byteLength, whole)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Fetch (fetch)
import Parsing (fail, runParserT)
import Proto.Messages as Proto
import ProtoHelper (fromMessage, sayError)
import System (IslandsSystemData)

-- * Stream requests

-- | Get the current stream state and update appropriately.
fetchStreamStatus :: (Boolean -> Effect Unit) -> (Int -> Effect Unit) -> Effect Unit
fetchStreamStatus setStreamStatus setStreamStateId = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio"
  launchAff_ do
    { arrayBuffer } <- fetch requestUrl { method: GET }
    body <- whole <$> arrayBuffer
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseGetRadioStatusResponse (byteLength body)
      case resp of
        Proto.GetRadioStatusResponse { radioOn: Just state, stateId: Just id } -> pure $ Tuple state id
        Proto.GetRadioStatusResponse { radioOn: Just _, stateId: Nothing } -> fail "Missing stateId"
        Proto.GetRadioStatusResponse { radioOn: Nothing, stateId: Just id } -> pure $ Tuple false id
        Proto.GetRadioStatusResponse { radioOn: Nothing, stateId: Nothing } -> fail "Missing all data"
    case result of
      Left err -> liftEffect $ Console.logShow err
      Right (Tuple state id) -> do
        liftEffect $ setStreamStatus state
        liftEffect $ setStreamStateId id
    pure unit

-- | Start or stop the audio stream.
modifyStream :: Ref.Ref Int -> (Boolean -> Effect Unit) -> Boolean -> Effect Unit
modifyStream ref setStreamStatus bool = do
  id <- Ref.read ref
  apiUrl <- getApiUrl
  let
    requestUrl = apiUrl <> "radio/modify" <> "?stateId=" <> show id
    body = Proto.mkModifyRadioRequest { start: Just bool }
  bodyBuff <- execPutM $ Proto.putModifyRadioRequest body
  launchAff_ do
    _ <- fetch requestUrl
      { method: POST
      , body: bodyBuff
      , headers: { "content-type": "application/protobuf" }
      }
    pure unit
    liftEffect $ fetchStreamStatus setStreamStatus (\n -> Ref.write n ref)


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
