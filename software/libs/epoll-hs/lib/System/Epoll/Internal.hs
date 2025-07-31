{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module System.Epoll.Internal where

import Data.Word (Word32, Word64)
import Foreign (Storable (..), (.|.))
import Foreign.C (CInt (..), CUInt (..))
import Foreign.Ptr (FunPtr, Ptr)
import System.Posix.Types (Fd)

foreign import capi safe "sys/epoll.h epoll_create"
  c_epoll_create :: CInt -> IO CInt

foreign import capi safe "sys/epoll.h epoll_create1"
  c_epoll_create1 :: CInt -> IO CInt

foreign import capi unsafe "unistd.h &close"
  c_epoll_close :: FunPtr (Ptr Fd -> IO ())

data EpollEvent = EpollEvent
  { events :: {-# UNPACK #-} !Word32
  , dataRaw :: {-# UNPACK #-} !Word64 -- just stores the union raw
  }

data EpollEventCode
  = EpollIn
  | EpollPri
  | EpollOut
  | EpollRdNorm
  | EpollRdBand
  | EpollWrNorm
  | EpollWrBand
  | EpollMsg
  | EpollErr
  | EpollHup
  | EpollRdHup
  | EpollExclusive
  | EpollWakeUp
  | EpollOneshot
  | EpollEt

epollEventCodeToWord :: EpollEventCode -> Word32
epollEventCodeToWord code =
  fromIntegral $ case code of
    EpollIn -> c_EPOLLIN
    EpollPri -> c_EPOLLPRI
    EpollOut -> c_EPOLLOUT
    EpollRdNorm -> c_EPOLLRDNORM
    EpollRdBand -> c_EPOLLRDBAND
    EpollWrNorm -> c_EPOLLWRNORM
    EpollWrBand -> c_EPOLLWRBAND
    EpollMsg -> c_EPOLLMSG
    EpollErr -> c_EPOLLERR
    EpollHup -> c_EPOLLHUP
    EpollRdHup -> c_EPOLLRDHUP
    EpollExclusive -> c_EPOLLEXCLUSIVE
    EpollWakeUp -> c_EPOLLWAKEUP
    EpollOneshot -> c_EPOLLONESHOT
    EpollEt -> c_EPOLLET

combineEpollEventCodes :: Foldable f => f EpollEventCode -> Word32
combineEpollEventCodes =
  foldl (\cur prev -> cur .|. epollEventCodeToWord prev) 0

instance Storable EpollEvent where
  sizeOf _ = 12 -- 4 (events) + 8 (data)
  alignment _ = alignment (undefined :: Word64)

  peek ptr = do
    ev <- peekByteOff ptr 0
    dr <- peekByteOff ptr 4
    return $ EpollEvent ev dr

  poke ptr (EpollEvent ev dr) = do
    pokeByteOff ptr 0 ev
    pokeByteOff ptr 4 dr

foreign import capi safe "sys/epoll.h epoll_wait"
  c_epoll_wait ::
    CInt
    -> Ptr EpollEvent
    -> CInt
    -> CInt
    -> IO CInt

foreign import capi safe "sys/epoll.h epoll_wait"
  c_epoll_wait_unsafe ::
    CInt
    -> Ptr EpollEvent
    -> CInt
    -> CInt
    -> IO CInt

foreign import capi "sys/epoll.h value EPOLL_CTL_ADD"
  c_EPOLL_CTL_ADD :: CInt

foreign import capi "sys/epoll.h value EPOLL_CTL_DEL"
  c_EPOLL_CTL_DEL :: CInt

foreign import capi "sys/epoll.h value EPOLL_CTL_MOD"
  c_EPOLL_CTL_MOD :: CInt

foreign import capi safe "sys/epoll.h epoll_ctl"
  c_epoll_ctl ::
    CInt
    -- ^ epfd
    -> CInt
    -- ^ op
    -> CInt
    -- ^ fd
    -> Ptr EpollEvent
    -- ^ event
    -> IO CInt

foreign import capi "sys/epoll.h value EPOLLIN"
  c_EPOLLIN :: CUInt

foreign import capi "sys/epoll.h value EPOLLPRI"
  c_EPOLLPRI :: CUInt

foreign import capi "sys/epoll.h value EPOLLOUT"
  c_EPOLLOUT :: CUInt

foreign import capi "sys/epoll.h value EPOLLRDNORM"
  c_EPOLLRDNORM :: CUInt

foreign import capi "sys/epoll.h value EPOLLRDBAND"
  c_EPOLLRDBAND :: CUInt

foreign import capi "sys/epoll.h value EPOLLWRNORM"
  c_EPOLLWRNORM :: CUInt

foreign import capi "sys/epoll.h value EPOLLWRBAND"
  c_EPOLLWRBAND :: CUInt

foreign import capi "sys/epoll.h value EPOLLMSG"
  c_EPOLLMSG :: CUInt

foreign import capi "sys/epoll.h value EPOLLERR"
  c_EPOLLERR :: CUInt

foreign import capi "sys/epoll.h value EPOLLHUP"
  c_EPOLLHUP :: CUInt

foreign import capi "sys/epoll.h value EPOLLRDHUP"
  c_EPOLLRDHUP :: CUInt

foreign import capi "sys/epoll.h value EPOLLEXCLUSIVE"
  c_EPOLLEXCLUSIVE :: CUInt

foreign import capi "sys/epoll.h value EPOLLWAKEUP"
  c_EPOLLWAKEUP :: CUInt

foreign import capi "sys/epoll.h value EPOLLONESHOT"
  c_EPOLLONESHOT :: CUInt

foreign import capi "sys/epoll.h value EPOLLET"
  c_EPOLLET :: CUInt

-- data SigSet

-- data Timespec

-- foreign import capi safe "sys/epoll.h epoll_pwait"
--   epoll_pwait_c ::
--     CInt
--     -> Ptr EpollEvent_t
--     -> CInt
--     -> CInt
--     -> Ptr SigSet
--     -> IO CInt

-- foreign import capi safe "sys/epoll.h epoll_pwait2"
--   epoll_pwait2_c ::
--     CInt
--     -> Ptr EpollEvent_t
--     -> CInt
--     -> Ptr Timespec
--     -> Ptr SigSet
--     -> IO CInt
