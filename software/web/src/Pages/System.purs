module Pages.System where

import Prelude

import Data.Maybe (Maybe(..))
import Data.UInt as UInt
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.Hooks ((<#~>))
import FRP.Poll (Poll)
import Patternfly (dlistGroup)
import System (DeviceData(..))

type SystemPageState =
  { home ::
      { systemData :: Poll (Maybe DeviceData)
      , chart :: Nut
      }
  , proxy ::
      { systemData :: Poll (Maybe DeviceData)
      , chart :: Nut
      }
  }

-- renderDiskCapacity :: Number -> Number -> String
-- renderDiskCapacity freeSpaceGB totalSpaceGB =
--   show freeSpaceGB <> " / " <> show totalSpaceGB <> " GB"

makeIslandDeviceDataCard :: Poll (Maybe DeviceData) -> Nut
makeIslandDeviceDataCard systemDataMPoll =
  systemDataMPoll <#~> case _ of
    Nothing -> DD.text_ "NOTHING"
    Just (DeviceData { cpuData, inDockerContainer, operatingSystemName, architecture, memTotalKb }) ->
      DD.dl [ DA.klass_ "pf-v5-c-description-list pf-m-horizontal" ]
        [ dlistGroup "CPU Model Name" <<< pure $ case cpuData of
            Nothing -> "-"
            Just ({ modelName }) -> modelName
        , dlistGroup "OS" $ pure operatingSystemName
        , dlistGroup "Architecture" $ pure architecture
        , dlistGroup "Container" <<< pure <<< show @Boolean $ inDockerContainer
        , dlistGroup "RAM" <<< pure $ case memTotalKb of
            Nothing -> "-"
            Just n -> (show $ (UInt.toNumber $ UInt.round $ (UInt.toNumber n / 1000.0) / 1000.0)) <> " GB"
        ]

makeIslandMemoryChartCard :: Nut -> Nut
makeIslandMemoryChartCard chart =
  DD.div [ DA.klass_ "pf-v5-l-grid__item", DA.style_ "height: 100%;" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card__body pf-m-display-lg pf-m-full-height" ]
        [ chart ]
    ]

makeIslandCardBody :: Poll (Maybe DeviceData) -> Nut -> Nut
makeIslandCardBody systemDataPoll chart =
  DD.div [ DA.klass_ "pf-v5-l-grid pf-m-all-6-col-on-lg pf-m-all--col-on-md" ]
    [ makeIslandDeviceDataCard systemDataPoll
    , makeIslandMemoryChartCard chart
    ]

systemPage :: SystemPageState -> Nut
systemPage { home, proxy } = Deku.do
  DD.div [ DA.klass_ "pf-v5-l-grid pf-m-gutter pf-m-all-6-col-on-lg pf-m-all-12-col-on-md" ]
    [ dataCard "Home Island" home.systemData home.chart
    , dataCard "Proxy Island" proxy.systemData proxy.chart
    ]
  where
  dataCard islandName systemDataPoll chartDataPoll =
    DD.div [ DA.klass_ "pf-v5-l-grid__item" ]
      [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
          [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
              [ DD.h1 [ DA.klass_ "pf-v5-c-card__title-text" ]
                  [ DC.text_ islandName ]
              , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
                  [ makeIslandCardBody systemDataPoll chartDataPoll
                  ]
              ]
          ]
      ]
