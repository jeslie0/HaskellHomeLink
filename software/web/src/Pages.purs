module Pages
  ( Page(..)
  , pageList
  , resetPollers
  , module Pages.Applications
  , module Pages.Overview
  , module Pages.System
  ) where

import Prelude

import Effect (Effect)
import Pages.Applications (ApplicationsPageState, applicationsPage, initialApplicationsPageState)
import Pages.Overview (OverviewPageState, overviewPage)
import Pages.System (SystemPageState, systemPage)
import Requests (AppPollers)

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

resetPollers :: AppPollers -> Page -> Effect Unit
resetPollers pollers =
  case _ of
    Overview -> do
      pollers.streamStatusPoller.start
      pollers.logsPoller.stop
      pollers.memoryDataPoller.stop
      pollers.systemsDataPoller.stop

    Logs -> do
      pollers.streamStatusPoller.stop
      pollers.logsPoller.start
      pollers.memoryDataPoller.stop
      pollers.systemsDataPoller.stop

    System -> do
      pollers.streamStatusPoller.stop
      pollers.logsPoller.stop
      pollers.memoryDataPoller.start
      pollers.systemsDataPoller.start
