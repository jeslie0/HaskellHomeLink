{-# LANGUAGE OverloadedStrings #-}
module Error.Generic where
import Error (Error(..))

data GenericError =
  FailedToFindElement

instance Error GenericError where
  errorString FailedToFindElement = "Failed to find element"
