{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module HIO.Error.ErrorCategory (ErrorCategory (..), GenericCategory) where

import Data.Text qualified as T

class ErrorCategory eCat where
  getErrorCategoryName :: T.Text

data GenericCategory

instance ErrorCategory GenericCategory where
  getErrorCategoryName = "generic"
