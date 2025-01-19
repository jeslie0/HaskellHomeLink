module Pages.Logs (logsPage, LogsPageState) where

import Prelude

import Api (Api)
import Chart (defaultChartOptions)
import Data.Array as Array
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (foldl)
import Data.UInt (fromInt)
import Data.UInt as UInt
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Self as Self
import Deku.Hooks ((<#~>))
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Logs (Log(..))
import Patternfly (dlistGroup)
import Protobuf.Internal.Prelude (toInt)
import System (IslandSystemData(..), IslandsSystemData(..), SystemData(..))
import Unsafe.Coerce (unsafeCoerce)

type LogsPageState = { api :: Api }

logsPage :: LogsPageState -> Nut
logsPage { api } =
  DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
        [ DD.h1 [ DA.klass_ "pf-v5-c-card__title-text" ]
            [ DC.text_ "Logs" ]
        ]
    , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
        [ api.logging.logsPoll <#~> \logs -> if Array.null logs then DD.text_ "No logs" else body ]
    , DD.footer [ DA.klass_ "pf-v5-c-card__footer" ]
        [ api.logging.logsPoll <#~> \logs -> if Array.null logs then DD.div [] [] else pagination ]
    ]
  where
  body =
    DD.table
      [ DA.klass_ "pf-v5-c-table pf-m-grid-md pf-m-striped"
      , DA.role_ "grid"
      ]
      [ DD.thead [ DA.klass_ "pf-v5-c-table__thead" ]
          [ DD.tr [ DA.klass_ "pr-v5-c-table__tr", DA.role_ "row" ]
              [ DD.th [ DA.klass_ "pf-v5-c-table__th", DA.role_ "columnheader", DA.scope_ "col" ]
                  [ DD.text_ "Time" ]
              , DD.th [ DA.klass_ "pf-v5-c-table__th", DA.role_ "columnheader", DA.scope_ "col" ]
                  [ DD.text_ "Island" ]
              , DD.th [ DA.klass_ "pf-v5-c-table__th", DA.role_ "columnheader", DA.scope_ "col" ]
                  [ DD.text_ "Log level" ]
              , DD.th [ DA.klass_ "pf-v5-c-table__th", DA.role_ "columnheader", DA.scope_ "col" ]
                  [ DD.text_ "Message" ]
              ]
          ]
      , api.logging.logsPoll <#~> \logs ->
          DD.tbody [ DA.klass_ "pf-v5-c-table__tbody", DA.role_ "rowgroup" ]
            ( logs
                <#> \(Log log) ->
                  DD.tr [ DA.klass_ "pf-v5-c-table__tr", DA.role_ "row" ]
                    [ makeCell "DATE"
                    , makeCell $ show log.logLevel
                    , makeCell $ show log.island
                    , makeCell $ foldl (\curr prev -> curr <> "\n " <> prev) "" log.messages
                    ]
            )

      ]

  makeCell content =
    DD.td [ DA.klass_ "pf-v5-c-table_td", DA.role_ "cell" ] [ DD.text_ content ]

  pagination =
    DD.div [ DA.klass_ "pf-v5-c-pagination" ]
      [ DD.div [ DA.klass_ "pf-v5-c-pagination__total-items" ]
          [ DD.b__ "1 - 10"
          , DD.text_ "&nbsp;of&nbsp"
          , DD.b__ "36"
          ]
      , DD.button [ DA.klass_ "pf-v5-c-menu-toggle pf-m-plain pf-m-text" ]
          [ DD.span [ DA.klass_ "pf-v5-c-menu-toggle__text" ]
              [ DD.b__ "1 - 10"
              , DD.text_ "    of    "
              , DD.b__ "36"
              ]
          , DD.span [ DA.klass_ "pf-v5-c-menu-toggle__controls" ]
              [ DD.span [ DA.klass_ "pf-v5-c-menu-toggle__togle-icon" ]
                  [ DD.i [ DA.klass_ "fas fa-caret-down", DA.ariaHidden_ "true" ] [] ]
              ]
          ]
      , DD.nav [ DA.klass_ "pf-v5-c-pagination__nav" ]
          [ DD.div [ DA.klass_ "pf-v5-c-pagination__nav-control pf-m-first" ]
              [ DD.button [ DA.klass_ "pf-v5-c-button pf-m-plain" ]
                  [ DD.i [ DA.klass_ "fas fa-angle-double-left" ] [] ]
              ]
          , DD.div [ DA.klass_ "pf-v5-c-pagination__nav-control pf-m-prev" ]
              [ DD.button [ DA.klass_ "pf-v5-c-button pf-m-plain" ]
                  [ DD.i [ DA.klass_ "fas fa-angle-left" ] [] ]
              ]
          , DD.div [ DA.klass_ "pf-v5-c-pagination__nav-page-select" ]
              [ DD.span [ DA.klass_ "pf-v5-c-form-control" ]
                  [ DD.input [ DA.xtype_ "number", DA.min_ "1", DA.max_ "4", DA.value_ "1" ] [] ]
              , DD.span__ "of 4"
              ]
          , DD.div [ DA.klass_ "pf-v5-c-pagination__nav-control pf-m-mext" ]
              [ DD.button [ DA.klass_ "pf-v5-c-button pf-m-plain" ]
                  [ DD.i [ DA.klass_ "fas fa-angle-right" ] [] ]
              ]
          , DD.div [ DA.klass_ "pf-v5-c-pagination__nav-control pf-m-prev" ]
              [ DD.button [ DA.klass_ "pf-v5-c-button pf-m-plain" ]
                  [ DD.i [ DA.klass_ "fas fa-angle-double-right" ] [] ]
              ]
          ]
      ]
