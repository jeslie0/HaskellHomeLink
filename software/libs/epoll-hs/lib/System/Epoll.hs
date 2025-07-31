module System.Epoll (Epoll, epollCreate, epollCreate1, epollToFd, epollWait, epollCtl, EpollCtlOp(..)) where

import Foreign (
  ForeignPtr,
  IntPtr (IntPtr),
  allocaArray,
  intPtrToPtr,
  newForeignPtr,
  peekArray,
  ptrToIntPtr,
  withForeignPtr,
 )
import Foreign.C (Errno, getErrno)
import Foreign.Marshal.Utils (with)
import System.Epoll.Internal (
  EpollEvent,
  c_EPOLL_CTL_ADD,
  c_EPOLL_CTL_DEL,
  c_EPOLL_CTL_MOD,
  c_epoll_close,
  c_epoll_create,
  c_epoll_create1,
  c_epoll_ctl,
  c_epoll_wait,
 )
import System.Posix.Types (Fd (..))

newtype Epoll = Epoll (ForeignPtr Fd)

-- | Extract the underlying file descriptor for the epoll
-- instance. Make sure that the epoll instance hasn't been GC's when
-- using the returned file descriptor.
epollToFd :: Epoll -> IO Fd
epollToFd (Epoll frnPtr) = do
  withForeignPtr frnPtr $ pure . fromIntegral . ptrToIntPtr

-- | Creates an epoll instance. The size parameter is a hint
-- specifying the number of file descriptors to be associated with the
-- new instance.
epollCreate :: Int -> IO (Maybe Epoll)
epollCreate size = do
  epfd <- c_epoll_create (fromIntegral size)
  if epfd < 0
    then pure Nothing
    else do
      Just . Epoll
        <$> newForeignPtr c_epoll_close (intPtrToPtr . IntPtr . fromIntegral $ epfd)

-- | Same as epollCreate but with an FLAGS parameter.
epollCreate1 :: Int -> IO (Maybe Epoll)
epollCreate1 flags = do
  epfd <- c_epoll_create1 (fromIntegral flags)
  if epfd < 0
    then pure Nothing
    else do
      Just . Epoll
        <$> newForeignPtr c_epoll_close (intPtrToPtr . IntPtr . fromIntegral $ epfd)

data EpollCtlOp = EpollCtlAdd | EpollCtlDelete | EpollCtlModify
  deriving (Eq, Show)

-- | Manipulate an epoll instance "epfd". Returns Right () in case of success,
-- Left err in the case of an error. The "fd" parameter is the target of the
-- operation. The "event" parameter describes which events the caller
-- is interested in and any associated user data.
epollCtl ::
  Epoll
  -> EpollCtlOp
  -> Fd
  -> EpollEvent
  -> IO (Either Errno ())
epollCtl epoll epollOp (Fd fd) event = do
  (Fd epfd) <- epollToFd epoll
  let op = case epollOp of
        EpollCtlAdd -> c_EPOLL_CTL_ADD
        EpollCtlDelete -> c_EPOLL_CTL_DEL
        EpollCtlModify -> c_EPOLL_CTL_MOD
  with event $ \evPtr -> do
    n <- c_epoll_ctl epfd op fd evPtr
    if n == 0
      then pure $ Right ()
      else Left <$> getErrno

-- | Wait for events on an epoll instance "epoll". Returns a list of
-- triggered events. The size of the list is capped by maxEvents. The
-- timeout parameter is the maximum number of milliseconds to wait,
-- with -1 being infinite.
epollWait :: Epoll -> Int -> Int -> IO (Maybe [EpollEvent])
epollWait epoll maxEvents timeout = do
  (Fd epfd) <- epollToFd epoll
  allocaArray maxEvents $ \eventsPtr -> do
    n <-
      c_epoll_wait epfd eventsPtr (fromIntegral maxEvents) (fromIntegral timeout)
    if n < 0
      then pure Nothing
      else Just <$> peekArray (fromIntegral n) eventsPtr
