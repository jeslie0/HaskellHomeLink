{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module HAsio.Fd.Socket.Internal where

import Data.Void (Void)
import Foreign.C (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import System.Posix.Types (CSsize (..))

foreign import capi unsafe "sys/socket.h send"
  c_send_unsafe :: CInt -> Ptr Void -> CSize -> CInt -> IO CSsize

foreign import capi safe "sys/socket.h send"
  c_send :: CInt -> Ptr Void -> CSize -> CInt -> IO CSsize

foreign import capi "sys/socket.h value MSG_CONFIRM"
  c_MSG_CONFIRM :: CInt

foreign import capi "sys/socket.h value MSG_DONTROUTE"
  c_MSG_DONTROUTE :: CInt

foreign import capi "sys/socket.h value MSG_DONTWAIT"
  c_MSG_DONTWAIT :: CInt

foreign import capi "sys/socket.h value MSG_EOR"
  c_MSG_EOR :: CInt

foreign import capi "sys/socket.h value MSG_MORE"
  c_MSG_MORE :: CInt

foreign import capi "sys/socket.h value MSG_NOSIGNAL"
  c_MSG_NOSIGNAL :: CInt

foreign import capi "sys/socket.h value MSG_OOB"
  c_MSG_OOB :: CInt

foreign import capi "sys/socket.h value MSG_FASTOPEN"
  c_MSG_FASTOPEN :: CInt
