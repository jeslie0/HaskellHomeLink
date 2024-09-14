module Main where

import Prelude

import Data.Tuple (Tuple(..))
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
import Effect.Console as Console
import FRP.Event (Event)
import FRP.Poll (Poll)
import KlassList as DA
import Pages

main :: Effect Unit
main = do
  _ <- runInBody dekuApp
  pure unit

header :: Poll Page -> (Page -> Effect Unit) -> Nut
header ev setPage =
  DD.header [ DA.klass_ "pf-v5-c-masthead" ]
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

            System -> systemPage states.systemPageState

            Applications -> applicationsPage states.applicationsPageState
        ]
    ]

type PageStates =
  { overviewPageState :: Poll OverviewPageState
  , systemPageState :: Poll SystemPageState
  , applicationsPageState :: Poll ApplicationsPageState
  }

dekuApp :: Nut
dekuApp = Deku.do
  Tuple setOverviewPageState overviewPageState <- DH.useHot initialOverviewPageState
  Tuple setSystemPageState systemPageState <- DH.useHot initialSystemPageState
  Tuple setApplicationsPageState applicationsPageState <- DH.useHot initialApplicationsPageState

  Tuple setPage page <- DH.useState Overview
  let
    changePage newPage = do
      Console.logShow newPage
      setPage newPage

  DD.div [ DA.klass_ "pf-v5-c-page" ]
    [ header page changePage
    , pageBody page { overviewPageState, systemPageState, applicationsPageState }
    ]
