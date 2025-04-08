module Main (main) where

import Api (mkApi)
import Data.Tuple.Nested ((/\))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks ((<#~>))
import Deku.Hooks as DH
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Poll (Poll)
import Pages (OverviewPageState, Page(..), SystemPageState, overviewPage, pageList, systemPage)
import Pages.Logs (LogsPageState, logsPage)
import Prelude (Unit, bind, pure, show, unit, ($), (<#>), (<>), (==))

main :: Effect Unit
main = do
  app <- dekuApp
  _ <- runInBody app
  pure unit

header :: Poll Page -> (Page -> Effect Unit) -> Nut
header ev setPage =
  DD.header [ DA.klass_ "pf-v5-c-masthead pf-m-display-inline" ]
    [ DD.div [ DA.klass_ "pf-v5-c-masthead__main" ] []
    , DD.div [ DA.klass_ "pf-v5-c-masthead__content" ]
        [ DD.nav [ DA.klass_ "pf-v5-c-nav pf-m-horizontal" ]
            [ DD.ul [ DA.klass_ "pf-v5-c-nav__list", DA.role_ "list" ]
                navList
            ]
        ]
    ]
  where
  navList = pageList <#> \page ->
    DD.li
      [ DA.klass $ ev <#> \activePage -> "pf-v5-c-nav__link" <> if page == activePage then " pf-m-current" else ""
      , DL.click_ $ \_ -> setPage page
      ]
      [ DC.text_ $ show page ]

pageBody :: Poll Page -> PageStates -> Nut
pageBody pagePoll states =
  DD.main [ DA.klass_ "pf-v5-c-page__main" ]
    [ DD.section [ DA.klass_ "pf-v5-c-page__main-section" ]
        [ pagePoll <#~> case _ of
            Overview -> overviewPage states.overviewPageState

            Logs -> logsPage states.logsPageState

            _ -> systemPage states.systemPageState
        ]
    ]

type PageStates =
  { overviewPageState :: OverviewPageState
  , logsPageState :: LogsPageState
  , systemPageState :: SystemPageState
  }

dekuApp :: Effect Nut
dekuApp = do
  api <- mkApi

  pure Deku.do
    setPage /\ page <- DH.useState Overview
    let
      changePage newPage = do
        _ <- case newPage of
          System -> do
            _ <- api.charters.home.subscribe api.islandState.home.memoryChartOptions
            _ <- api.charters.proxy.subscribe api.islandState.proxy.memoryChartOptions

            _ <- api.pollers.systemsDataPoller.force
            _ <- api.pollers.memoryDataPoller.force
            pure unit

          _ -> do
            _ <- api.pollers.memoryDataPoller.stop
            _ <- api.pollers.systemsDataPoller.stop
            _ <- api.charters.home.unsubscribe
            _ <- api.charters.proxy.unsubscribe
            pure unit

        setPage newPage

    DD.div [ DA.klass_ "pf-v5-c-page" ]
      [ header page changePage
      , pageBody page
          { overviewPageState: { api }
          , systemPageState:
              { home:
                  { systemData: api.islandState.home.systemData
                  , chart: api.islandState.home.chart
                  }
              , proxy:
                  { systemData: api.islandState.proxy.systemData
                  , chart: api.islandState.proxy.chart
                  }
              }
          , logsPageState: { logsPoll: api.logging.logsPoll }
          }
      ]
