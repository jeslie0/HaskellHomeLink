module Error where

import Data.Text qualified as T
import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSON(..))
import GHC.Generics (Generic)

class Error a where
  errorString :: a -> T.Text

data ErrorType = forall a. (Error a) => ErrorType a

instance ToJSON ErrorType where
  toJSON (ErrorType a) = toJSON $ errorString a

newtype ErrorStack = ErrorStack [ErrorType] deriving Generic

instance ToJSON ErrorStack

push :: forall a. (Error a) => a -> ErrorStack -> ErrorStack
push err (ErrorStack stack) = ErrorStack (ErrorType err : stack)

toEitherErrorStack :: (Error a) => Either a b -> Either ErrorStack b
toEitherErrorStack (Left a) = Left $ ErrorStack [ErrorType a]
toEitherErrorStack (Right b) = Right b

fromMaybe :: (Error a) => a -> Maybe b -> Either a b
fromMaybe err Nothing = Left err
fromMaybe _ (Just b) = Right b
