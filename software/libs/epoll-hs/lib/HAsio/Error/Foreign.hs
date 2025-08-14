{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module HAsio.Error.Foreign (c_strerror, strError) where

import Data.Text qualified as T
import Foreign.C (CInt (..), Errno (..))
import Foreign.C.String (CString, peekCString)
import System.IO.Unsafe ( unsafePerformIO )

foreign import capi unsafe "string.h strerror"
  c_strerror :: CInt -> IO CString

strError :: Errno -> T.Text
strError (Errno err) =
  unsafePerformIO $ do
    cstr <- c_strerror err
    T.pack <$> peekCString cstr
