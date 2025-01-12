module Pages.System where

import Prelude

import Apexcharts (createChartEl, render)
import Apexcharts.Chart as C
import Apexcharts.Chart.Zoom as Z
import Apexcharts.Common as CC
import Apexcharts.DataLabels as DL
import Apexcharts.NoData as ND
import Apexcharts.Series as SE
import Apexcharts.Stroke as S
import Apexcharts.Tooltip as TT
import Apexcharts.Xaxis as X
import Apexcharts.Xaxis.Title as XT
import Api (Api)
import Data.Map as Map
import Data.Options ((:=))
import Data.Time.Duration (Milliseconds(..))
import Data.UInt (fromInt)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Self as Self
import Deku.Hooks ((<#~>))
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Patternfly (dlistGroup)
import Protobuf.Internal.Prelude (toInt)
import System (IslandSystemData(..), IslandsSystemData(..), islands)
import Unsafe.Coerce (unsafeCoerce)

type SystemPageState = { api :: Api }

renderDiskCapacity :: Number -> Number -> String
renderDiskCapacity freeSpaceGB totalSpaceGB =
  show freeSpaceGB <> " / " <> show totalSpaceGB <> " GB"

mkIslandSystemDataCard :: IslandSystemData -> Nut
mkIslandSystemDataCard (IslandSystemData { island, systemData }) =
  DD.div [ DA.klass_ "pf-v5-l-grid__item" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
            [ DD.h1 [ DA.klass_ "pf-v5-c-card__title-text" ]
                [ DC.text_ $ show island ]
            ]
        , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
            [ descriptionList ]
        ]
    ]
  where
  descriptionList =
    DD.dl [ DA.klass_ "pf-v5-c-description-list pf-m-horizontal" ]
      [ dlistGroup "CPU Model Name" $ pure (unsafeCoerce systemData).cpuData.modelName
      , dlistGroup "OS" $ pure (unsafeCoerce systemData).operatingSystemName
      , dlistGroup "Architecture" $ pure (unsafeCoerce systemData).architecture
      , dlistGroup "Container" <<< pure <<< show @Boolean $ (unsafeCoerce systemData).inDockerContainer
      , dlistGroup "RAM" <<< pure $ (show @Int <<< toInt $ (unsafeCoerce systemData).memTotalKb / fromInt 1000000) <> " GB"
      ]

systemPage :: SystemPageState -> Nut
systemPage { api } =
  DD.div []
    [ api.polls.systemsDataPoll <#~> \(IslandsSystemData { allSystemData }) ->
        DD.div
          [ DA.klass_ "pf-v5-l-grid pf-m-gutter pf-m-all-4-col-on-lg pf-m-all-4-col-on-md" ] $
          mkIslandSystemDataCard <$> allSystemData
    , charts
    ]
  where
  charts =
    DD.div [ DA.klass_ "pf-v5-l-grid pf-m-gutter pf-m-all-6-col-on-lg pf-m-all-12-col-on-md" ] $ islands <#> \island ->
      DD.div [ DA.klass_ "pf-v5-l-grid__item" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h1 [ DA.klass_ "pf-v5-c-card__title-text" ]
                    [ DC.text_ $ show island ]
                ]
            , chartCard island
            ]
        ]
  chartCard island =
    DD.div [ DA.klass_ "pf-v5-c-card__body" ]
      [ DD.div
          [ Self.self_ $ \el ->
              launchAff_ do
                delay (Milliseconds 0.0)
                liftEffect $ Console.log "hi there"
                liftEffect do
                  c <- createChartEl el
                    ( C.chart :=
                        ( C.type' := CC.Area
                            <> C.height := 350.0
                            <> C.width := "100%"
                            <> Z.zoom := (Z.enabled := false)
                        )
                        <> TT.tooltip := (TT.enabled := false)
                        <> DL.dataLabels := (DL.enabled := false)
                        <> S.stroke := S.curve := S.Smooth
                        <> X.xaxis :=
                          ( XT.title := (XT.text := "Time")
                              <> X.type' := X.Datetime
                          )
                        <> SE.series @Number := []
                        <> ND.noData := ND.text := "Loading..."
                    )
                  render c
                  chartMap <- Ref.read api.memoryCharts.apexchartsRef
                  Ref.write (Map.insert island c chartMap) api.memoryCharts.apexchartsRef
          ]
          []
      ]
