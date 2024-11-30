module Pages.Overview (OverviewPageState, overviewPage, fetchStreamStatus) where

import Prelude

import Constants (getApiUrl)
import Data.ArrayBuffer.Builder (execPutM)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Hooks ((<#~>))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Poll (Poll)
import Fetch (fetch)
import Effect.Ref (Ref)
import Effect.Ref as Ref
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
          , DL.click_ $ \_ -> stopStream setStreamActivePoll
          ]
          [ DD.text_ "Stop radio" ]
      ]

fetchStreamStatus :: (Boolean -> Effect Unit) -> (Int -> Effect Unit) -> Effect Unit
fetchStreamStatus setStreamStatus setStreamStateId = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio"
  launchAff_ do
    { json } <- fetch requestUrl { method: GET }
    respData <- json
    let (mBool :: Maybe Boolean) = read_ respData
    case mBool of
      Nothing -> pure unit
      Just bool -> liftEffect $ setStreamStatus bool

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
    -- liftEffect $ fetchStreamStatus setStreamStatus

stopStream :: (Boolean -> Effect Unit) -> Effect Unit
stopStream setStreamStatus = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio/modify"
  launchAff_ do
    _ <- fetch requestUrl { method: PUT }
    pure unit
    -- liftEffect $ fetchStreamStatus setStreamStatus
