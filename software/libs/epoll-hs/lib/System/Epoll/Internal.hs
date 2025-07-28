{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Epoll.Internal where

import Data.Void (Void)
import Foreign.C (CInt (..), CUInt (..), CULong (..))
import Foreign.Ptr (FunPtr, Ptr)

data EpollEvent

foreign import capi safe "epoll_hs.h make_epoll_event_voidptr_c"
  make_epoll_event_voidptr_c :: CUInt -> Ptr Void -> IO (Ptr EpollEvent)

foreign import capi safe "epoll_hs.h make_epoll_event_int_c"
  make_epoll_event_int_c :: CUInt -> CInt -> IO (Ptr EpollEvent)

foreign import capi safe "epoll_hs.h make_epoll_event_uint32_c"
  make_epoll_event_uint32_c :: CUInt -> CULong -> IO (Ptr EpollEvent)

foreign import capi safe "epoll_hs.h make_epoll_event_uint64_c"
  make_epoll_event_uint64_c :: CUInt -> CULong -> IO (Ptr EpollEvent)

foreign import capi safe "epoll_hs.h &free_epoll_event_c"
  free_epoll_event_c :: FunPtr (Ptr EpollEvent -> IO ())

foreign import capi safe "sys/epoll.h epoll_create"
  epoll_create_c :: CInt -> IO CInt

foreign import capi safe "sys/epoll.h epoll_create1"
  epoll_create1_c :: CInt -> IO CInt

foreign import capi safe "sys/epoll.h epoll_ctl"
  epoll_ctl_c ::
    CInt
    -> CInt
    -> CInt
    -> Ptr EpollEvent
    -> IO CInt

foreign import capi safe "sys/epoll.h epoll_wait"
  epoll_wait_c ::
    CInt
    -> Ptr EpollEvent
    -> CInt
    -> CInt
    -> IO CInt

-- data SigSet

-- data Timespec

-- foreign import capi safe "sys/epoll.h epoll_pwait"
--   epoll_pwait_c ::
--     CInt
--     -> Ptr EpollEvent
--     -> CInt
--     -> CInt
--     -> Ptr SigSet
--     -> IO CInt

-- foreign import capi safe "sys/epoll.h epoll_pwait2"
--   epoll_pwait2_c ::
--     CInt
--     -> Ptr EpollEvent
--     -> CInt
--     -> Ptr Timespec
--     -> Ptr SigSet
--     -> IO CInt
