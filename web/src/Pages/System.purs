module Pages.System (systemPage, SystemPageState, initialSystemPageState) where

import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import FRP.Poll (Poll)

type SystemPageState =
  { state :: Int
  }

initialSystemPageState :: SystemPageState
initialSystemPageState = { state: 0 }

systemPage :: Poll SystemPageState -> Nut
systemPage _ = DD.div [] [ DC.text_ "System Page" ]
