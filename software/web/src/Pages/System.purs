module Pages.System where

import Prelude

import Api (Api)
import Data.UInt (UInt, fromInt)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.Hooks ((<#~>))
import Patternfly (dlistGroup)
import Protobuf.Internal.Prelude (toInt)
import System (IslandSystemData(..), IslandsSystemData(..))
import Unsafe.Coerce (unsafeCoerce)

type SystemPageState = { api :: Api }

renderDiskCapacity :: Number -> Number -> String
renderDiskCapacity freeSpaceGB totalSpaceGB = show freeSpaceGB <> " / " <> show totalSpaceGB <> " GB"

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
      , dlistGroup "Ram (MB)" <<< pure <<< show @Int <<< toInt $ (unsafeCoerce systemData).memTotalKb / fromInt 1000
      ]

systemPage :: SystemPageState -> Nut
systemPage { api } =
  api.polls.systemsDataPoll <#~> \(IslandsSystemData { allSystemData }) ->
    DD.div
      [ DA.klass_ "pf-v5-l-grid pf-m-gutter pf-m-all-3-col-on-lg pf-m-all-6-col-on-md" ] $
       mkIslandSystemDataCard <$> allSystemData
