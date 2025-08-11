{-# LANGUAGE OverloadedStrings #-}

module HIO.Error.ErrorStack (SomeError (..), ErrorStack (..), push, toText, singleton, pushErrno) where

import Data.Functor ((<&>))
import Data.Text qualified as T
import Foreign.C (getErrno)
import HIO.Error.Error (Error (..), getErrorCategoryNameFromError)
import Control.Exception (Exception)

data SomeError = forall err. Error err => SomeError err

newtype ErrorStack = ErrorStack {getStack :: [SomeError]}

instance Show ErrorStack where
  show = T.unpack . toText

instance Exception ErrorStack

instance Semigroup ErrorStack where
  (ErrorStack errs1) <> (ErrorStack errs2) =
    ErrorStack (errs1 <> errs2)

instance Monoid ErrorStack where
  mempty = ErrorStack []

push :: Error err => err -> ErrorStack -> ErrorStack
push err (ErrorStack errs) =
  ErrorStack (SomeError err : errs)

singleton :: Error err => err -> ErrorStack
singleton err = ErrorStack [SomeError err]

pushErrno :: Error err => err -> IO ErrorStack
pushErrno err = do
  errno <- getErrno
  pure $ err `push` (errno `push` mempty)

toText :: ErrorStack -> T.Text
toText (ErrorStack errs) =
  T.intercalate
    "\n"
    ( errs <&> \(SomeError err) -> "[" <> getErrorCategoryNameFromError err <> "] " <> getErrorMessage err
    )
