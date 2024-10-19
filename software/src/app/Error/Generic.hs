{-# LANGUAGE OverloadedStrings #-}
module Error.Generic where
import Control.Exception (Exception(..))
import Data.Data (Typeable)

data GenericError =
  FailedToFindElement
  | OtherError
  deriving (Typeable, Show)

instance Exception GenericError where
  displayException FailedToFindElement = "Failed to find element"
  displayException OtherError = "An unknown error occurred"
