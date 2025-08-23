{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HAsio.Error.Error (Error (..), getErrorCategoryNameFromError) where

import Data.Kind (Type)
import Data.Text qualified as T
import Foreign.C (Errno (..))
import HAsio.Error.ErrorCategory (ErrorCategory (..), GenericCategory)
import HAsio.Error.Foreign (strError)

class ErrorCategory (ECat err) => Error err where
  type ECat err :: Type

  getErrorMessage :: err -> T.Text

getErrorCategoryNameFromError :: forall err. Error err => err -> T.Text
getErrorCategoryNameFromError _ = HAsio.Error.ErrorCategory.getErrorCategoryName @(ECat err)

instance Error Errno where
  type ECat Errno = GenericCategory

  getErrorMessage (Errno n) = "(" <> (T.pack . show $ n) <> ") " <> strError (Errno n)
