module Pages.Overview (OverviewPageState, overviewPage, fetchStreamStatus) where

import Prelude

import Constants (getApiUrl)
import Data.ArrayBuffer.Builder (execPutM)
import Data.ArrayBuffer.DataView (whole, byteLength)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Hooks ((<#~>))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FRP.Poll (Poll)
import Fetch (fetch)
import JS.Fetch.Response (arrayBuffer)
import Parsing (fail, runParserT)
import Proto.Proxy as Proto
import Yoga.JSON (read_)

type OverviewPageState = { setStreamActivePoll :: Boolean -> Effect Unit, streamActivePoll :: Poll Boolean, streamStateIdRef :: Ref Int }

overviewPage :: Poll OverviewPageState -> Nut
overviewPage pollState = do
  pollState <#~> \ { setStreamActivePoll, streamActivePoll, streamStateIdRef } ->
    DD.div
      [ DA.klass_ "pf-v5-l-gallery pf-m-gutter" ]
      [ DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]
          [ DD.div [ DA.klass_ "pf-v5-c-card" ]
              [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                  [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                      [ DC.text_ "Radio" ]
                  ]
              , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
                  [ overviewPageBody setStreamActivePoll streamActivePoll streamStateIdRef]
              , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ DC.text_ "Footer" ]
              ]
          ]
      ]
  where
  overviewPageBody setStreamActivePoll streamActivePoll streamStateIdRef=
    DD.div []
      [ DD.button
          [ DA.klass_ "pf-v5-c-button pf-m-primary"
          , DA.disabled $ streamActivePoll <#> if _ then "true" else ""
          , DL.click_ $ \_ -> startStream streamStateIdRef setStreamActivePoll
          ]
          [ DD.text_ "Start radio"
          ]
      , DD.button
          [ DA.klass_ "pf-v5-c-button pf-m-primary"
          , DA.disabled $ streamActivePoll <#> if _ then "" else "true"
          , DL.click_ $ \_ -> stopStream streamStateIdRef setStreamActivePoll
          ]
          [ DD.text_ "Stop radio" ]
      ]

fetchStreamStatus :: (Boolean -> Effect Unit) -> (Int -> Effect Unit) -> Effect Unit
fetchStreamStatus setStreamStatus setStreamStateId = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio"
  launchAff_ do
    { arrayBuffer } <- fetch requestUrl { method: GET }
    body <- whole <$> arrayBuffer
    result <- liftEffect $ runParserT body  do
      resp <- Proto.parseGetRadioStatusResponse (byteLength body)
      case resp of
        Proto.GetRadioStatusResponse {radioOn: Just state, stateId: Just id} -> pure $ Tuple state id
        Proto.GetRadioStatusResponse {radioOn: Just state, stateId: Nothing} -> fail "Missing stateId"
        Proto.GetRadioStatusResponse {radioOn: Nothing, stateId: Just id} -> pure $ Tuple false id
        Proto.GetRadioStatusResponse {radioOn: Nothing, stateId: Nothing} -> fail "Missing all data"
        _ -> fail "Missing required entries"
    case result of
      Left err -> liftEffect $ Console.logShow err
      Right (Tuple state id) -> do
        liftEffect $ setStreamStatus state
        liftEffect $ setStreamStateId id
    pure unit
      -- Left a -> pure 1
      -- Right a -> pure 2
    -- let (mBool :: Maybe Boolean) = read_ respData
    -- case mBool of
    --   Nothing -> pure unit
    --   Just bool -> liftEffect $ setStreamStatus bool

startStream :: Ref Int -> (Boolean -> Effect Unit) -> Effect Unit
startStream ref setStreamStatus = do
  id <- Ref.read ref
  apiUrl <- getApiUrl
  let
    requestUrl = apiUrl <> "radio/modify" <> "?stateId=" <> show id
    body = Proto.mkModifyRadioRequest { start: Just true }
  bodyBuff <- execPutM $ Proto.putModifyRadioRequest body
  launchAff_ do
    _ <- fetch requestUrl { method: POST
                          , body: bodyBuff
                          , headers: { "content-type": "application/protobuf" }
                          }
    pure unit
    liftEffect $ fetchStreamStatus setStreamStatus (\n -> Ref.write n ref)

stopStream :: Ref Int -> (Boolean -> Effect Unit) -> Effect Unit
stopStream ref setStreamStatus = do
  id <- Ref.read ref
  apiUrl <- getApiUrl
  let
    requestUrl = apiUrl <> "radio/modify" <> "?stateId=" <> show id
    body = Proto.mkModifyRadioRequest { start: Just false }
  bodyBuff <- execPutM $ Proto.putModifyRadioRequest body
  launchAff_ do
    _ <- fetch requestUrl { method: POST
                          , body: bodyBuff
                          , headers: { "content-type": "application/protobuf" }
                          }
    pure unit
    liftEffect $ fetchStreamStatus setStreamStatus (\n -> Ref.write n ref)
