module Requests
  ( fetchStreamStatus
  , modifyStream
  , fetchSystemsData
  ) where

import Prelude

import Constants (getApiUrl)
import Data.Array as Array
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
import ProtoHelper (fromMessage, sayError, toMessage)
import Radio (Stream, radioStreams)
import System (IslandsSystemData)

-- * Stream requests

-- | Get the current stream state and update appropriately.
fetchStreamStatus :: (Boolean -> Effect Unit) -> (Int -> Effect Unit) -> (Stream -> Effect Unit) -> Effect Unit
fetchStreamStatus setStreamActiveStatus setStreamStateId setStreamStatus = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio"
  launchAff_ do
    { arrayBuffer } <- fetch requestUrl { method: GET }
    body <- whole <$> arrayBuffer
    result <- liftEffect $ runParserT body do
      resp <- Proto.parseGetRadioStatusResponse (byteLength body)
      case resp of
        Proto.GetRadioStatusResponse { currentStream: mStream, stateId: Just id } -> pure $ Tuple mStream id
        Proto.GetRadioStatusResponse { currentStream: _, stateId: Nothing } -> fail "Missing all stateId"
    case result of
      Left err -> liftEffect $ Console.logShow err
      Right (Tuple mStream id) -> do
        liftEffect $ setStreamStateId id
        liftEffect $ case mStream of
          Nothing -> setStreamActiveStatus false
          Just eStream -> do
            case fromMessage eStream of
              Left err -> setStreamActiveStatus false >>= \_ -> Console.logShow <<< sayError $ err
              Right stream -> do
                setStreamActiveStatus true
                setStreamStatus $ stream
    pure unit

-- | Start or stop the audio stream.
modifyStream :: Ref.Ref Int -> Ref.Ref Stream -> (Boolean -> Effect Unit) -> (Stream -> Effect Unit) -> Boolean -> Effect Unit
modifyStream ref streamRef setStreamStatus setSelectedStream bool = do
  id <- Ref.read ref
  stream <- Ref.read streamRef
  let mRStream = Array.find (\rstream -> rstream.stream == stream) radioStreams
  apiUrl <- getApiUrl
  let
    requestUrl = apiUrl <> "radio/modify" <> "?stateId=" <> show id
    body = Proto.mkModifyRadioRequest { start: Just bool
                                      , url: mRStream <#> _.url
                                      , newStream: mRStream <#> toMessage <<< _.stream }
  bodyBuff <- execPutM $ Proto.putModifyRadioRequest body
  launchAff_ do
    _ <- fetch requestUrl
      { method: POST
      , body: bodyBuff
      , headers: { "content-type": "application/protobuf" }
      }
    pure unit
    liftEffect $ fetchStreamStatus setStreamStatus (\n -> Ref.write n ref) setSelectedStream

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
