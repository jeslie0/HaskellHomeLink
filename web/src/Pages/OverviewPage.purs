module Pages.Overview (OverviewPageState, initialOverviewPageState, overviewPage) where

import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import FRP.Poll (Poll)

type OverviewPageState =
  { state :: Int
  }

initialOverviewPageState :: OverviewPageState
initialOverviewPageState = { state: 0 }

overviewPage :: Poll OverviewPageState -> Nut
overviewPage _ =
  DD.div [] [ DC.text_ "Overview Page"]
