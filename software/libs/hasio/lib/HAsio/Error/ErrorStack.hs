{-# LANGUAGE OverloadedStrings #-}

module HAsio.Error.ErrorStack (
  SomeError (..),
  ErrorStack (..),
  push,
  toText,
  HAsio.Error.ErrorStack.singleton,
  pushErrno,
  makeErrorStack,
  getBaseErr,
  useErrorStack,
  getBaseErrno,
) where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT (..))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..), last, singleton, toList, (<|))
import Data.Text qualified as T
import Data.Typeable (Typeable, cast)
import Foreign.C (Errno(..), getErrno)
import HAsio.Error.Error (Error (..), getErrorCategoryNameFromError)

data SomeError = forall err. (Error err, Typeable err) => SomeError err
  deriving Typeable

instance Show SomeError where
  show (SomeError err) = show . getErrorMessage $ err

newtype ErrorStack = ErrorStack {getStack :: NonEmpty SomeError}

instance Show ErrorStack where
  show = T.unpack . toText

instance Exception ErrorStack

instance Semigroup ErrorStack where
  (ErrorStack errs1) <> (ErrorStack errs2) =
    ErrorStack (errs1 <> errs2)

push :: (Error err, Typeable err) => err -> ErrorStack -> ErrorStack
push err (ErrorStack errs) =
  ErrorStack (SomeError err <| errs)

singleton :: (Error err, Typeable err) => err -> ErrorStack
singleton err = ErrorStack . Data.List.NonEmpty.singleton $ SomeError err

pushErrno :: (Error err, Typeable err) => err -> IO ErrorStack
pushErrno err = do
  errno <- getErrno
  pure $ err `push` HAsio.Error.ErrorStack.singleton errno

toText :: ErrorStack -> T.Text
toText (ErrorStack errs) =
  T.intercalate
    "\n"
    ( toList errs <&> \(SomeError err) -> "[" <> getErrorCategoryNameFromError err <> "] " <> getErrorMessage err
    )

makeErrorStack ::
  (Error err, Typeable err, Applicative m) => err -> ExceptT ErrorStack m ()
makeErrorStack = ExceptT . pure . Left . HAsio.Error.ErrorStack.singleton

getBaseErr :: ErrorStack -> SomeError
getBaseErr (ErrorStack errs) = Data.List.NonEmpty.last errs

getBaseErrno :: ErrorStack -> Maybe Errno
getBaseErrno errs =
  case getBaseErr errs of
    SomeError err -> cast err

useErrorStack :: Applicative m => ErrorStack -> ExceptT ErrorStack m a
useErrorStack = ExceptT . pure . Left
