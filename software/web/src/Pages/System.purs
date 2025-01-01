module Pages.System where

import Prelude

import Api (Api)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.Hooks ((<#~>))
import Patternfly (dlistGroup, gallery)
import System (IslandSystemData(..), IslandsSystemData(..))
import Unsafe.Coerce (unsafeCoerce)

type SystemPageState = { api :: Api }

renderDiskCapacity :: Number -> Number -> String
renderDiskCapacity freeSpaceGB totalSpaceGB = show freeSpaceGB <> " / " <> show totalSpaceGB <> " GB"

mkIslandSystemDataCard :: IslandSystemData -> Nut
mkIslandSystemDataCard (IslandSystemData { island, systemData }) =
  DD.div [ DA.klass_ "pf-v5-c-card pf-m-full-height" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
        [ DD.h1 [ DA.klass_ "pf-v5-c-card__title-text" ]
            [ DC.text_ $ show island ]
        ]
    , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
        [ descriptionList ]
    ]
  where
  descriptionList =
    DD.dl [ DA.klass_ "pf-v5-c-description-list pf-m-horizontal" ]
      [ dlistGroup "CPU Model Name" $ pure (unsafeCoerce systemData).cpuData.modelName
      , dlistGroup "Vendor" $ pure (unsafeCoerce systemData).cpuData.vendor
      , dlistGroup "Container" $ pure $ show @Boolean $ (unsafeCoerce systemData).inDockerContainer
      -- , dlistGroup "CPU Cores" $ infoPoll <#> \info -> fromMaybe "-" $ show <$> info.cpuCores
      -- , dlistGroup "CPU Threads" $ infoPoll <#> \info -> fromMaybe "-" $ show <$> info.cpuThreads
      -- , dlistGroup "Disk Capacity" $ infoPoll <#> \info -> fromMaybe "-" $ renderDiskCapacity <$> info.freeSpaceGB <*> info.totalSpaceGB
      ]


systemPage :: SystemPageState -> Nut
systemPage { api } =
  api.polls.systemsDataPoll <#~> \ (IslandsSystemData {allSystemData}) ->
    gallery $ mkIslandSystemDataCard <$> allSystemData
