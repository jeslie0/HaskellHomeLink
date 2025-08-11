{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module HIO.Error.Error (Error (..), getErrorCategoryNameFromError) where

import Data.Kind (Type)
import Data.Text qualified as T
import Foreign.C (Errno (..))
import HIO.Error.ErrorCategory (ErrorCategory (..), GenericCategory)
import HIO.Error.Foreign (strError)

class ErrorCategory (ECat err) => Error err where
  type ECat err :: Type

  getErrorMessage :: err -> T.Text

getErrorCategoryNameFromError :: forall err. Error err => err -> T.Text
getErrorCategoryNameFromError _ = HIO.Error.ErrorCategory.getErrorCategoryName @(ECat err)

instance Error Errno where
  type ECat Errno = GenericCategory

  getErrorMessage = strError
