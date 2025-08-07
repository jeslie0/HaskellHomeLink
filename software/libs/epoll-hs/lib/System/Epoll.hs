module System.Epoll (
  Epoll,
  Event (..),
  Flag (..),
  System.Epoll.EpollEvent (..),
  EpollCtlOp (..),
  epollToFd,
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

import Control.Exception (IOException, throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Word (Word64)
import Foreign (
  ForeignPtr,
  IntPtr (IntPtr),
  allocaArray,
  intPtrToPtr,
  newForeignPtr,
  peekArray,
  ptrToIntPtr,
  withForeignPtr,
  (.|.),
 )
import Foreign.C (Errno (Errno), errnoToIOError, getErrno)
import Foreign.Marshal.Utils (with)
import System.Epoll.Internal (
  EpollEvent (..),
  Event (..),
  Flag (..),
  c_EPOLL_CTL_ADD,
  c_EPOLL_CTL_DEL,
  c_EPOLL_CTL_MOD,
  c_epoll_close,
  c_epoll_create,
  c_epoll_create1,
  c_epoll_ctl,
  c_epoll_wait,
  combineEvents,
  combineFlags,
  word32ToEvents,
 )
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd (..))

newtype Epoll = Epoll (ForeignPtr Fd)

-- | Extract the underlying file descriptor for the epoll
-- instance. Make sure that the epoll instance hasn't been GC's when
-- using the returned file descriptor.
epollToFd :: Epoll -> Fd
epollToFd (Epoll frnPtr) = do
  unsafePerformIO . withForeignPtr frnPtr $ pure . fromIntegral . ptrToIntPtr

-- | Creates an epoll instance. The size parameter is a hint
-- specifying the number of file descriptors to be associated with the
-- new instance.
epollCreate :: Int -> IO (Either IOException Epoll)
epollCreate size = do
  epfd <- c_epoll_create (fromIntegral size)
  if epfd < 0
    then
      pure . Left $
        errnoToIOError "epollCreate" (Errno epfd) Nothing Nothing
    else do
      frnPtr <-
        liftIO $
          newForeignPtr c_epoll_close (intPtrToPtr . IntPtr . fromIntegral $ epfd)
      pure . Right $ Epoll frnPtr

epollCreate' :: Int -> ExceptT IOException IO Epoll
epollCreate' = ExceptT . epollCreate

-- | The same as 'epollCreate' by can throw an 'IOException'.
epollCreate_ :: Int -> IO Epoll
epollCreate_ =
  either throwIO pure <=< epollCreate

-- | Same as 'epollCreate' but with an FLAGS parameter.
epollCreate1 :: Int -> IO (Either IOException Epoll)
epollCreate1 flags = do
  epfd <- c_epoll_create1 (fromIntegral flags)
  if epfd < 0
    then
      pure . Left $
        errnoToIOError "epollCreate1" (Errno epfd) Nothing Nothing
    else do
      frnPtr <-
        liftIO $
          newForeignPtr c_epoll_close (intPtrToPtr . IntPtr . fromIntegral $ epfd)
      pure . Right $ Epoll frnPtr

epollCreate1' :: Int -> ExceptT IOException IO Epoll
epollCreate1' =
  ExceptT . epollCreate1

-- | The same as 'epollCreate1' but can throw an 'IOException'.
epollCreate1_ :: Int -> IO Epoll
epollCreate1_ =
  either throwIO pure <=< epollCreate1

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

toInternalEvent :: System.Epoll.EpollEvent -> System.Epoll.Internal.EpollEvent
toInternalEvent (System.Epoll.EpollEvent events flags dataRaw) =
  System.Epoll.Internal.EpollEvent
    (combineEvents events .|. combineFlags flags)
    dataRaw

fromInternalEvent :: System.Epoll.Internal.EpollEvent -> System.Epoll.EpollEvent
fromInternalEvent (System.Epoll.Internal.EpollEvent events dataRaw) =
  System.Epoll.EpollEvent
    (word32ToEvents events)
    []
    dataRaw

-- | Manipulate an epoll instance "epfd". Returns an IOException in
-- the case of an error. The "fd" parameter is the target of the
-- operation. The "event" parameter describes which events the caller
-- is interested in and any associated user data.
epollCtl ::
  Epoll
  -> EpollCtlOp
  -> Fd
  -> System.Epoll.EpollEvent
  -> IO (Either IOException ())
epollCtl epoll epollOp (Fd fd) event = do
  let
    Fd epfd = epollToFd epoll
    op = case epollOp of
      EpollCtlAdd -> c_EPOLL_CTL_ADD
      EpollCtlDelete -> c_EPOLL_CTL_DEL
      EpollCtlModify -> c_EPOLL_CTL_MOD

  with (toInternalEvent event) $ \evPtr -> do
    n <- c_epoll_ctl epfd op fd evPtr
    if n == 0
      then pure $ Right ()
      else do
        er <- getErrno
        pure . Left $ errnoToIOError "epollCtl" er Nothing Nothing

epollCtl' ::
  Epoll
  -> EpollCtlOp
  -> Fd
  -> System.Epoll.EpollEvent
  -> ExceptT IOException IO ()
epollCtl' epoll epollOp fd =
  ExceptT . epollCtl epoll epollOp fd

-- | Same as 'epollCtl' but can throw an 'IOException'.
epollCtl_ ::
  Epoll
  -> EpollCtlOp
  -> Fd
  -> System.Epoll.EpollEvent
  -> IO ()
epollCtl_ epoll epollOp fd =
  either throwIO pure <=< epollCtl epoll epollOp fd

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
  -> IO (Either IOException [System.Epoll.EpollEvent])
epollWait epoll maxEvents timeout = do
  let Fd epfd = epollToFd epoll
  allocaArray maxEvents $ \eventsPtr -> do
    n <-
      c_epoll_wait epfd eventsPtr (fromIntegral maxEvents) (fromIntegral timeout)
    if n < 0
      then do
        er <- getErrno
        pure . Left $ errnoToIOError "epollWait" er Nothing Nothing
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
  -> ExceptT IOException IO [System.Epoll.EpollEvent]
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
  -> IO [System.Epoll.EpollEvent]
epollWait_ epoll maxEvents = do
  either throwIO pure <=< epollWait epoll maxEvents
