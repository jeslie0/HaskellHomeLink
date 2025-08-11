{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module HIO.Fd.EventFd.Internal where

import Foreign.C (CInt (..), CUInt (..))

foreign import capi safe "sys/eventfd.h eventfd"
  c_eventfd :: CUInt -> CInt -> IO CInt

-- * Flags

foreign import capi "sys/eventfd.h value EFD_CLOEXEC"
  c_EFD_CLOEXEC :: CInt

foreign import capi "sys/eventfd.h value EFD_NONBLOCK"
  c_EFD_NONBLOCK :: CInt

foreign import capi "sys/eventfd.h value EFD_SEMAPHORE"
  c_EFD_SEMAPHORE :: CInt

