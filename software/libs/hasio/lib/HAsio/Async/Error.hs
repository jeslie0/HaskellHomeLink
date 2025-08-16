{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HAsio.Async.Error where

import HAsio.Error.Error (Error (..))
import HAsio.Error.ErrorCategory (ErrorCategory (..))

data AsyncError
  = MissingCallback
  | FailedToMakeIOContext
  | EventNotRegistered

data AsyncErrorCategory

instance ErrorCategory AsyncErrorCategory where
  getErrorCategoryName = "async"

instance Error AsyncError where
  type ECat AsyncError = AsyncErrorCategory

  getErrorMessage err =
    case err of
      MissingCallback -> "Missing callback in callback table"
      FailedToMakeIOContext -> "Failed to create IO Context"
      EventNotRegistered -> "Event not registered"
