{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIO.Async.Error where

import HIO.Error.Error (Error (..))
import HIO.Error.ErrorCategory (ErrorCategory (..))

data AsyncError
  = MissingCallback

data AsyncErrorCategory

instance ErrorCategory AsyncErrorCategory where
  getErrorCategoryName = "async"

instance Error AsyncError where
  type ECat AsyncError = AsyncErrorCategory

  getErrorMessage err =
    case err of
      MissingCallback -> "Missing callback in callback table"
