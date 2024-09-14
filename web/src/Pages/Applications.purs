module Pages.Applications (applicationsPage, initialApplicationsPageState, ApplicationsPageState) where

import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import FRP.Poll (Poll)

type ApplicationsPageState =
  { state :: Int
  }

initialApplicationsPageState :: ApplicationsPageState
initialApplicationsPageState = { state: 0 }

applicationsPage :: Poll ApplicationsPageState -> Nut
applicationsPage _ = DD.div [] [ DC.text_ "Applications Page" ]
