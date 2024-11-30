module Pages.Overview (OverviewPageState, overviewPage, fetchStreamStatus) where

import Prelude

import Constants (getApiUrl)
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
import Yoga.JSON (read_)

type OverviewPageState = { setStreamActivePoll :: Boolean -> Effect Unit, streamActivePoll :: Poll Boolean }

overviewPage :: Poll OverviewPageState -> Nut
overviewPage pollState = do
  pollState <#~> \{ setStreamActivePoll, streamActivePoll } ->
    DD.div
      [ DA.klass_ "pf-v5-l-gallery pf-m-gutter" ]
      [ DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]
          [ DD.div [ DA.klass_ "pf-v5-c-card" ]
              [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                  [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                      [ DC.text_ "Radio" ]
                  ]
              , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
                  [ overviewPageBody setStreamActivePoll streamActivePoll ]
              , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ DC.text_ "Footer" ]
              ]
          ]
      ]
  where
  overviewPageBody setStreamActivePoll streamActivePoll =
    DD.div []
      [ DD.button
          [ DA.klass_ "pf-v5-c-button pf-m-primary"
          , DA.disabled $ streamActivePoll <#> if _ then "true" else ""
          , DL.click_ $ \_ -> startStream setStreamActivePoll
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

fetchStreamStatus :: (Boolean -> Effect Unit) -> Effect Unit
fetchStreamStatus setStreamStatus = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio"
  launchAff_ do
    { json } <- fetch requestUrl { method: GET }
    respData <- json
    let (mBool :: Maybe Boolean) = read_ respData
    case mBool of
      Nothing -> pure unit
      Just bool -> liftEffect $ setStreamStatus bool

startStream :: (Boolean -> Effect Unit) -> Effect Unit
startStream setStreamStatus = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio/start"
  launchAff_ do
    _ <- fetch requestUrl { method: PUT }
    liftEffect $ fetchStreamStatus setStreamStatus

stopStream :: (Boolean -> Effect Unit) -> Effect Unit
stopStream setStreamStatus = do
  apiUrl <- getApiUrl
  let requestUrl = apiUrl <> "radio/stop"
  launchAff_ do
    _ <- fetch requestUrl { method: PUT }
    liftEffect $ fetchStreamStatus setStreamStatus
