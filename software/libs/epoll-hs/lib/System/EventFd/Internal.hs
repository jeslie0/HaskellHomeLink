{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module System.EventFd.Internal where

import Foreign.C (CInt (..), CUInt (..))
import Foreign.Ptr (Ptr, FunPtr)
import System.Posix.Types (Fd)

foreign import capi safe "sys/eventfd.h eventfd"
  c_eventfd :: CUInt -> CInt -> IO CInt

-- * Flags

foreign import capi "sys/eventfd.h value EFD_CLOEXEC"
  c_EFD_CLOEXEC :: CInt

foreign import capi "sys/eventfd.h value EFD_NONBLOCK"
  c_EFD_NONBLOCK :: CInt

foreign import capi "sys/eventfd.h value EFD_SEMAPHORE"
  c_EFD_SEMAPHORE :: CInt

-- * Operations
-- read, write, poll, select, close

foreign import capi unsafe "unistd.h &close"
  c_eventfd_close :: FunPtr (Ptr Fd -> IO ())
