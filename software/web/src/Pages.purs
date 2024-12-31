module Pages
  ( Page(..)
  , pageList
  , module Pages.Applications
  , module Pages.Overview
  , module Pages.System
  ) where

import Prelude

import Pages.Applications (ApplicationsPageState, applicationsPage, initialApplicationsPageState)
import Pages.Overview (OverviewPageState, overviewPage)
import Pages.System (SystemPageState, systemPage)

data Page
  = Overview
  | System
  | Applications

pageList :: Array Page
pageList = [ Overview, Applications, System ]

derive instance eqPages :: Eq Page

instance Show Page where
  show Overview = "Overview"
  show System = "System"
  show Applications = "Applications"
