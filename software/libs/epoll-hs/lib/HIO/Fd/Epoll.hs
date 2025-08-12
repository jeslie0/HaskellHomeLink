module HIO.Fd.Epoll (
  Epoll (..),
  Event (..),
  Flag (..),
  HIO.Fd.Epoll.EpollEvent (..),
  EpollCtlOp (..),
  epollCreate,
  epollCreate',
  epollCreate_,
  epollCreate1,
  epollCreate1',
  epollCreate1_,
  epollWait,
  epollWait',
  epollWait_,
  epollCtl,
  epollCtl',
  epollCtl_,
) where

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Word (Word64)
import Foreign (
  allocaArray,
  peekArray,
  (.|.),
 )
import Foreign.C (Errno (Errno))
import Foreign.Marshal.Utils (with)
import HIO.Error.ErrorStack (ErrorStack, push, pushErrno)
import HIO.Error.Syscalls qualified as ESys
import HIO.Fd.Epoll.Internal (
  EpollEvent (..),
  Event (..),
  Flag (..),
  c_EPOLL_CTL_ADD,
  c_EPOLL_CTL_DEL,
  c_EPOLL_CTL_MOD,
  c_epoll_create,
  c_epoll_create1,
  c_epoll_ctl,
  c_epoll_wait,
  combineEvents,
  combineFlags,
  word32ToEvents,
 )
import HIO.Fd.IsFd (IsFd (..))
import System.Posix.Types (Fd (..))

newtype Epoll = Epoll Fd

instance IsFd Epoll where
  toFd (Epoll fd) = fd

  fromFd = Epoll

-- | Creates an epoll instance. The size parameter is a hint
-- specifying the number of file descriptors to be associated with the
-- new instance.
epollCreate :: Int -> IO (Either ErrorStack Epoll)
epollCreate size = do
  epfd <- c_epoll_create (fromIntegral size)
  pure $
    if epfd < 0
      then
        Left $ ESys.EpollCreate `push` (Errno epfd `push` mempty)
      else do
        Right $ Epoll (Fd epfd)

epollCreate' :: Int -> ExceptT ErrorStack IO Epoll
epollCreate' =
  ExceptT . epollCreate

-- | The same as 'epollCreate' by can throw an 'IOException'.
epollCreate_ :: Int -> IO Epoll
epollCreate_ =
  either
    throwIO
    pure
    <=< epollCreate

-- | Same as 'epollCreate' but with an FLAGS parameter.
epollCreate1 :: Int -> IO (Either ErrorStack Epoll)
epollCreate1 flags = do
  epfd <- c_epoll_create1 (fromIntegral flags)
  pure $
    if epfd < 0
      then
        Left $ ESys.EpollCreate1 `push` (Errno epfd `push` mempty)
      else do
        Right $ Epoll (Fd epfd)

epollCreate1' :: Int -> ExceptT ErrorStack IO Epoll
epollCreate1' =
  ExceptT . epollCreate1

-- | The same as 'epollCreate1' but can throw an 'IOException'.
epollCreate1_ :: Int -> IO Epoll
epollCreate1_ =
  either
    throwIO
    pure
    <=< epollCreate1

data EpollCtlOp
  = EpollCtlAdd
  | EpollCtlDelete
  | EpollCtlModify
  deriving (Eq, Show)

data EpollEvent = EpollEvent
  { events :: [Event]
  , flags :: [Flag]
  , dataRaw :: !Word64 -- just stores the union raw
  }

toInternalEvent :: HIO.Fd.Epoll.EpollEvent -> HIO.Fd.Epoll.Internal.EpollEvent
toInternalEvent (HIO.Fd.Epoll.EpollEvent events flags dataRaw) =
  HIO.Fd.Epoll.Internal.EpollEvent
    (combineEvents events .|. combineFlags flags)
    dataRaw

fromInternalEvent :: HIO.Fd.Epoll.Internal.EpollEvent -> HIO.Fd.Epoll.EpollEvent
fromInternalEvent (HIO.Fd.Epoll.Internal.EpollEvent events dataRaw) =
  HIO.Fd.Epoll.EpollEvent
    (word32ToEvents events)
    []
    dataRaw

-- | Manipulate an epoll instance "epfd". Returns an IOException in
-- the case of an error. The "fd" parameter is the target of the
-- operation. The "event" parameter describes which events the caller
-- is interested in and any associated user data.
epollCtl ::
  IsFd fd =>
  Epoll
  -> EpollCtlOp
  -> fd
  -> HIO.Fd.Epoll.EpollEvent
  -> IO (Either ErrorStack ())
epollCtl epoll epollOp fd event = do
  let Fd epfd = toFd epoll
  let op = case epollOp of
        EpollCtlAdd -> c_EPOLL_CTL_ADD
        EpollCtlDelete -> c_EPOLL_CTL_DEL
        EpollCtlModify -> c_EPOLL_CTL_MOD

  with (toInternalEvent event) $ \evPtr -> do
    n <- c_epoll_ctl epfd op (fromIntegral . toFd $ fd) evPtr
    if n == 0
      then pure $ Right ()
      else do
        Left <$> pushErrno ESys.EpollCtl

epollCtl' ::
  IsFd fd =>
  Epoll
  -> EpollCtlOp
  -> fd
  -> HIO.Fd.Epoll.EpollEvent
  -> ExceptT ErrorStack IO ()
epollCtl' epoll epollOp fd =
  ExceptT . epollCtl epoll epollOp fd

-- | Same as 'epollCtl' but can throw an 'IOException'.
epollCtl_ ::
  IsFd fd =>
  Epoll
  -> EpollCtlOp
  -> fd
  -> HIO.Fd.Epoll.EpollEvent
  -> IO ()
epollCtl_ epoll epollOp fd =
  either
    throwIO
    pure
    <=< epollCtl epoll epollOp fd

-- | Wait for events on an epoll instance "epoll". Returns a list of
-- triggered events. The size of the list is capped by maxEvents. The
-- timeout parameter is the maximum number of milliseconds to wait,
-- with -1 being infinite.
epollWait ::
  Epoll
  -- ^ Epoll context
  -> Int
  -- ^ Maximum number of events to return
  -> Int
  -- ^ Timeout before returning
  -> IO (Either ErrorStack [HIO.Fd.Epoll.EpollEvent])
epollWait epoll maxEvents timeout = do
  let Fd epfd = toFd epoll
  allocaArray maxEvents $ \eventsPtr -> do
    n <-
      c_epoll_wait epfd eventsPtr (fromIntegral maxEvents) (fromIntegral timeout)
    if n < 0
      then do
        Left <$> pushErrno ESys.EpollWait
      else do
        arr <- peekArray (fromIntegral n) eventsPtr
        pure . Right $ (fromInternalEvent <$> arr)

epollWait' ::
  Epoll
  -- ^ Epoll context
  -> Int
  -- ^ Maximum number of events to return
  -> Int
  -- ^ Timeout before returning
  -> ExceptT ErrorStack IO [HIO.Fd.Epoll.EpollEvent]
epollWait' epoll maxEvents =
  ExceptT . epollWait epoll maxEvents

-- | Same as 'epollWait' but can throw an 'IOExcenption'
epollWait_ ::
  Epoll
  -- ^ Epoll context
  -> Int
  -- ^ Maximum number of events to return
  -> Int
  -- ^ Timeout before returning
  -> IO [HIO.Fd.Epoll.EpollEvent]
epollWait_ epoll maxEvents = do
  either
    throwIO
    pure
    <=< epollWait epoll maxEvents
