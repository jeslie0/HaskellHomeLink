module Pages.System where

import Prelude

import Apexcharts (createChartEl, render)
import Api (Api)
import Chart (defaultChartOptions)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.UInt (fromInt)
import Data.UInt as UInt
import Debug (trace)
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
import System (CPUData(..), IslandsSystemData(..), SystemData(..))
import Unsafe.Coerce (unsafeCoerce)

type SystemPageState = { api :: Api }

renderDiskCapacity :: Number -> Number -> String
renderDiskCapacity freeSpaceGB totalSpaceGB =
  show freeSpaceGB <> " / " <> show totalSpaceGB <> " GB"

mkIslandSystemDataCard :: SystemData -> Nut
mkIslandSystemDataCard (SystemData { cpuData, inDockerContainer, operatingSystemName, architecture, memTotalKb, island}) =
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
      [ dlistGroup "CPU Model Name" <<< pure $ case cpuData of
           Nothing -> "-"
           Just (CPUData {modelName}) -> modelName
      , dlistGroup "OS" $ pure operatingSystemName
      , dlistGroup "Architecture" $ pure architecture
      , dlistGroup "Container" <<< pure <<< show @Boolean $ inDockerContainer
      , dlistGroup "RAM" <<< pure $ case memTotalKb of
        Nothing -> "-"
        Just n -> (show $ (UInt.toNumber $ UInt.round $ (UInt.toNumber n / 1000.0) / 1000.0)) <> " GB"
      ]

mkIslandMemoryChartCard :: Api -> SystemData -> Nut
mkIslandMemoryChartCard api (SystemData systemData) =
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
        c <- createChartEl el (defaultChartOptions $ fromMaybe 0.0 $ UInt.toNumber <$> systemData.memTotalKb)
        render c
        chartMap <- Ref.read api.memoryCharts.apexchartsRef
        Ref.write (Map.insert systemData.island c chartMap) api.memoryCharts.apexchartsRef
        api.requests.getMemoryData

mkIslandGrid :: Api -> SystemData -> Nut
mkIslandGrid api systemData =
  DD.div [ DA.klass_ "pf-v5-l-grid pf-m-all-6-col-on-lg pf-m-all-12-col-on-md" ]
    [ mkIslandSystemDataCard systemData, mkIslandMemoryChartCard api systemData ]

systemPage :: SystemPageState -> Nut
systemPage { api } =
  DD.div []
    [ api.polls.systemsDataPoll <#~> \(IslandsSystemData { allSystemData }) ->
        if Array.null allSystemData then noDataCard
        else
          DD.div
            [ DA.klass_ "pf-v5-l-grid pf-m-gutter pf-m-all-12-col-on-lg pf-m-all-12-col-on-md" ] $
            allSystemData <#> \islandSystemData -> mkIslandGrid api islandSystemData
    ]
  where
  noDataCard =
    DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
      [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
          [ DD.h1 [ DA.klass_ "pf-v5-c-card__title-text" ]
              [ DC.text_ "No data" ]
          ]
      ]
