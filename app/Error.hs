{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Error where

import Control.Exception (Exception (..), try)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSON (..))
import GHC.Generics (Generic)

data Error = forall a. (Exception a) => Error a

instance ToJSON Error where
  toJSON (Error e) = toJSON $ displayException e

newtype ErrorStack = ErrorStack [Error] deriving (Generic)

instance ToJSON ErrorStack

push :: forall e. (Exception e) => e -> ErrorStack -> ErrorStack
push err (ErrorStack stack) = ErrorStack (Error err : stack)

toEitherErrorStack ::  Exception e => Either e b -> Either ErrorStack b
toEitherErrorStack (Left e) = Left $ ErrorStack [Error e]
toEitherErrorStack (Right b) = Right b

fromMaybe ::  Exception e => e -> Maybe b -> Either e b
fromMaybe err Nothing = Left err
fromMaybe _ (Just b) = Right b

tryError :: forall e a. (Exception e) => IO a -> IO (Either Error a)
tryError io = do
  eith <- try @e io
  return $ case eith of
    Left ex -> Left . Error $ ex
    Right a -> Right a
