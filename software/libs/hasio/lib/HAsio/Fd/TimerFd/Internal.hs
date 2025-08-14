{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module HAsio.Fd.TimerFd.Internal where

import Foreign (Storable (..))
import Foreign.C (CInt (..))
import Foreign.Ptr (FunPtr, Ptr)
import System.Clock (TimeSpec (..))
import System.Posix.Types (Fd)

foreign import capi safe "sys/timerfd.h timerfd_create"
  c_timerfd_create :: CInt -> CInt -> IO CInt

data ITimerSpec = ITimerSpec
  { it_interval :: TimeSpec -- Interval
  , it_value :: TimeSpec -- initial expiration
  }
  deriving (Show, Eq)

instance Storable ITimerSpec where
  sizeOf _ = 2 * sizeOf (undefined :: TimeSpec)
  alignment _ = alignment (undefined :: TimeSpec)

  peek ptr = do
    interval <- peekByteOff ptr 0
    value <- peekByteOff ptr (sizeOf (undefined :: TimeSpec))
    return $ ITimerSpec interval value

  poke ptr (ITimerSpec interval value) = do
    pokeByteOff ptr 0 interval
    pokeByteOff ptr (sizeOf (undefined :: TimeSpec)) value

foreign import capi safe "sys/timerfd.h timerfd_settime"
  c_timerfd_settime :: CInt -> CInt -> Ptr ITimerSpec -> Ptr ITimerSpec -> IO CInt

foreign import capi safe "sys/timerfd.h timerfd_gettime"
  c_timerfd_gettime :: CInt -> Ptr ITimerSpec -> IO CInt

-- * ClockId

foreign import capi "sys/timerfd.h value CLOCK_REALTIME"
  c_CLOCK_REALTIME :: CInt

foreign import capi "sys/timerfd.h value CLOCK_MONOTONIC"
  c_CLOCK_MONOTONIC :: CInt

foreign import capi "sys/timerfd.h value CLOCK_BOOTTIME"
  c_CLOCK_BOOTTIME :: CInt

foreign import capi "sys/timerfd.h value CLOCK_REALTIME_ALARM"
  c_CLOCK_REALTIME_ALARM :: CInt

foreign import capi "sys/timerfd.h value CLOCK_BOOTTIME_ALARM"
  c_CLOCK_BOOTTIME_ALARM :: CInt

-- * Flags

foreign import capi "sys/timerfd.h value TFD_NONBLOCK"
  c_TFD_NONBLOCK :: CInt

foreign import capi "sys/timerfd.h value TFD_CLOEXEC"
  c_TFD_CLOEXEC :: CInt

-- ** settime flags

foreign import capi "sys/timerfd.h value TFD_TIMER_ABSTIME"
  c_TFD_TIMER_ABSTIME :: CInt

foreign import capi "sys/timerfd.h value TFD_TIMER_CANCEL_ON_SET"
  c_TFD_TIMER_CANCEL_ON_SET :: CInt

-- * Operations

-- read, poll, select, ioctl, close

foreign import capi unsafe "unistd.h &close"
  c_timerfd_close :: FunPtr (Ptr Fd -> IO ())
