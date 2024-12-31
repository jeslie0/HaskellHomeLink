module Main where


import Api (mkApi)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes (klass, klass_, role_) as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Effect as DE
import Deku.Hooks ((<#~>))
import Deku.Hooks as DH
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import FRP.Poll (Poll)
import Pages (ApplicationsPageState, OverviewPageState, Page(..), SystemPageState, applicationsPage, initialApplicationsPageState, overviewPage, pageList, systemPage)
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<#>), (<>), (==))

main :: Effect Unit
main = do
  app <- dekuApp
  _ <- runInBody app
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

            _ -> systemPage states.systemPageState

            -- Applications -> applicationsPage states.applicationsPageState
        ]
    ]

type PageStates =
  { overviewPageState :: OverviewPageState
  , systemPageState :: SystemPageState
  -- , applicationsPageState :: ApplicationsPageState
  }

dekuApp :: Effect Nut
dekuApp = do
  api <- mkApi

  pure Deku.do
    setPage /\page <- DH.useState Overview
    let
      changePage newPage = do
        Console.logShow newPage
        setPage newPage

    DD.div [ DA.klass_ "pf-v5-c-page" ]
      [ header page changePage
      , pageBody page { overviewPageState: {api}, systemPageState: {api} }
      ]
