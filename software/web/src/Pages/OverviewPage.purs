module Pages.Overview (OverviewPageState, overviewPage) where

import Prelude

import Api (Api)
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
import Proto.Messages as Proto
import Yoga.JSON (read_)

type OverviewPageState = { api :: Api }

overviewPage :: OverviewPageState -> Nut
overviewPage {api} = do
    DD.div
      [ DA.klass_ "pf-v5-l-gallery pf-m-gutter" ]
      [ DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]
          [ DD.div [ DA.klass_ "pf-v5-c-card" ]
              [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                  [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                      [ DC.text_ "Radio" ]
                  ]
              , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
                  [ overviewPageBody  ]
              , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ DC.text_ "Footer" ]
              ]
          ]
      ]
  where
  overviewPageBody  =
    DD.div []
      [ DD.button
          [ DA.klass_ "pf-v5-c-button pf-m-primary"
          , DA.disabled $ api.polls.streamActivePoll <#> if _ then "true" else ""
          , DL.click_ $ \_ -> api.requests.modifyStream true
          ]
          [ DD.text_ "Start radio"
          ]
      , DD.button
          [ DA.klass_ "pf-v5-c-button pf-m-primary"
          , DA.disabled $ api.polls.streamActivePoll <#> if _ then "" else "true"
          , DL.click_ $ \_ -> api.requests.modifyStream false
          ]
          [ DD.text_ "Stop radio" ]
      ]
