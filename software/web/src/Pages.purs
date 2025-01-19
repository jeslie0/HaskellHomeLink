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
  | Logs
  | System

pageList :: Array Page
pageList = [ Overview, Logs, System ]

derive instance eqPages :: Eq Page

instance Show Page where
  show Overview = "Overview"
  show Logs = "Logs"
  show System = "System"
