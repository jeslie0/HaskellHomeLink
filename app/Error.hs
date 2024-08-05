module Error where

import Data.Text qualified as T

class Error a where
  errorString :: a -> T.Text

data ErrorType = forall a. (Error a) => ErrorType a

newtype ErrorStack = ErrorStack [ErrorType]

push :: forall a. (Error a) => a -> ErrorStack -> ErrorStack
push err (ErrorStack stack) = ErrorStack (ErrorType err : stack)
