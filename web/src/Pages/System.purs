module Pages.System (HardwareInformation, TemperatureInformation, systemPage, SystemPageState, initialSystemPageState, getAndSetSystemPageInfo) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (fixed, toStringWith)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Effect (Effect)
import Effect.Random (random)
import Effect.Timer (IntervalId, setInterval)
import FRP.Poll (Poll)
import Patternfly (dlistGroup, gallery, galleryItem)

getAndSetSystemPageInfo :: (SystemPageState -> Effect Unit) -> Effect IntervalId
getAndSetSystemPageInfo update = do
  setInterval 1000 do
    newInfo <- random <#> \n ->
      { hardwareInformation:
          { cpuModelName: Just "Intel(R) Core(TM) i7-8750H CPU @ 2.20GHz"
          , cpuCores: Just 6
          , cpuThreads: Just 12
          , totalSpaceGB: Just 512.0
          , freeSpaceGB: Just 100.0
          }
      , temperatureInformation:
          { cpuTemperatureC: Just (37.0 + 3.0 * n - 6.0)
          }
      }
    update newInfo

type HardwareInformation =
  { cpuModelName :: Maybe String
  , cpuCores :: Maybe Int
  , cpuThreads :: Maybe Int
  , totalSpaceGB :: Maybe Number
  , freeSpaceGB :: Maybe Number
  }

type TemperatureInformation =
  { cpuTemperatureC :: Maybe Number
  }

type SystemPageState =
  { hardwareInformation :: HardwareInformation
  , temperatureInformation :: TemperatureInformation
  }

renderDiskCapacity :: Number -> Number -> String
renderDiskCapacity freeSpaceGB totalSpaceGB = show freeSpaceGB <> " / " <> show totalSpaceGB <> " GB"

initialSystemPageState :: SystemPageState
initialSystemPageState =
  { hardwareInformation:
      { cpuModelName: Nothing
      , cpuCores: Nothing
      , cpuThreads: Nothing
      , totalSpaceGB: Nothing
      , freeSpaceGB: Nothing
      }
  , temperatureInformation:
      { cpuTemperatureC: Nothing
      }
  }

hardwareCard :: Poll HardwareInformation -> Nut
hardwareCard infoPoll =
  DD.div [ DA.klass_ "pf-v5-c-card pf-m-full-height" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
        [ DD.h1 [ DA.klass_ "pf-v5-c-card__title-text" ]
            [ DC.text_ "Hardware Information" ]
        ]
    , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
        [ descriptionList ]
    ]
  where
  descriptionList =
    DD.dl [ DA.klass_ "pf-v5-c-description-list pf-m-horizontal" ]
      [ dlistGroup "CPU Model Name" $ infoPoll <#> \info -> fromMaybe "-" info.cpuModelName
      , dlistGroup "CPU Cores" $ infoPoll <#> \info -> fromMaybe "-" $ show <$> info.cpuCores
      , dlistGroup "CPU Threads" $ infoPoll <#> \info -> fromMaybe "-" $ show <$> info.cpuThreads
      , dlistGroup "Disk Capacity" $ infoPoll <#> \info -> fromMaybe "-" $ renderDiskCapacity <$> info.freeSpaceGB <*> info.totalSpaceGB
      ]

temperatureCard :: Poll TemperatureInformation -> Nut
temperatureCard tempInfoPoll =
  DD.div [ DA.klass_ "pf-v5-c-card pf-m-full-height" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
        [ DD.h1 [ DA.klass_ "pf-v5-c-card__title-text" ]
            [ DC.text_ "Temperature Information" ]
        ]
    , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
        [ descriptionList ]
    ]
  where
  descriptionList =
    DD.dl [ DA.klass_ "pf-v5-c-description-list pf-m-horizontal" ]
      [ dlistGroup "CPU Temperature" $ tempInfoPoll <#>
          \info -> fromMaybe "-" $ info.cpuTemperatureC <#> \temp -> toStringWith (fixed 1) temp <> " C"
      ]

systemPage :: Poll SystemPageState -> Nut
systemPage systemPoll =
  gallery
    [ galleryItem [ hardwareCard $ _.hardwareInformation <$> systemPoll ]
    , galleryItem [ temperatureCard $ _.temperatureInformation <$> systemPoll ]
    ]
