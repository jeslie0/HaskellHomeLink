module Pages
  ( Page(..)
  , pageList
  , module Pages.Applications
  , module Pages.Overview
  , module Pages.System
  ) where

import Prelude

import Pages.Applications
import Pages.Overview
import Pages.System

data Page
  = Overview
  | System
  | Applications

pageList :: Array Page
pageList = [ Overview, System, Applications ]

derive instance eqPages :: Eq Page

instance Show Page where
  show Overview = "Overview"
  show System = "System"
  show Applications = "Applications"
