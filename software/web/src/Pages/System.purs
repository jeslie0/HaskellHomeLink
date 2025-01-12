module Pages.System where

import Prelude

import Apexcharts (createChartEl, render)
import Api (Api)
import Chart (defaultChartOptions)
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
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
import Patternfly (dlistGroup)
import Protobuf.Internal.Prelude (toInt)
import System (IslandSystemData(..), IslandsSystemData(..), SystemData(..))
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

mkIslandMemoryChartCard :: Api -> IslandSystemData -> Nut
mkIslandMemoryChartCard api (IslandSystemData {island, systemData: SystemData systemData}) =
  DD.div [ DA.klass_ "pf-v5-l-grid__item" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card__body" ]
            [ body ]
        ]
    ]
  where
  body =
    DD.div [ DA.klass_ "pf-v5-c-card__body" ]
      [ DD.div
          [ Self.self_ createChart ]
          []
      ]

  createChart el =
    launchAff_ do
      delay (Milliseconds 0.0)
      liftEffect do
        c <- createChartEl el (defaultChartOptions $ UInt.toNumber systemData.memTotalKb)
        render c
        chartMap <- Ref.read api.memoryCharts.apexchartsRef
        Ref.write (Map.insert island c chartMap) api.memoryCharts.apexchartsRef
        api.requests.getMemoryData

mkIslandGrid :: Api -> IslandSystemData -> Nut
mkIslandGrid api islandSystemData =
  DD.div [ DA.klass_ "pf-v5-l-grid pf-m-all-6-col-on-lg pf-m-all-12-col-on-md" ]
    [ mkIslandSystemDataCard islandSystemData, mkIslandMemoryChartCard api islandSystemData ]

systemPage :: SystemPageState -> Nut
systemPage { api } =
  DD.div []
    [ api.polls.systemsDataPoll <#~> \(IslandsSystemData { allSystemData }) ->
        DD.div
          [ DA.klass_ "pf-v5-l-grid pf-m-gutter pf-m-all-12-col-on-lg pf-m-all-12-col-on-md" ] $
          allSystemData <#> \islandSystemData -> mkIslandGrid api islandSystemData
    ]
