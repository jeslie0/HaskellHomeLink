module System.EventFd (
  EventFd,
  EventFdFlags (..),
  eventFdToFd,
  eventFd,
  eventFd',
  eventFd_,
  System.EventFd.read,
  read',
  read_,
  write,
  write',
  write_,
  close,
) where

import Control.Exception (IOException, throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Foldable (Foldable (foldl'))
import Foreign (
  ForeignPtr,
  Int64,
  IntPtr (IntPtr),
  Ptr,
  Storable (..),
  allocaArray,
  castPtr,
  finalizeForeignPtr,
  intPtrToPtr,
  newForeignPtr,
  ptrToIntPtr,
  withForeignPtr,
  (.|.),
 )
import Foreign.C (errnoToIOError, getErrno)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (with)
import System.EventFd.Internal (
  c_EFD_CLOEXEC,
  c_EFD_NONBLOCK,
  c_EFD_SEMAPHORE,
  c_eventfd,
  c_eventfd_close,
 )
import System.Posix.Internals (c_read, c_write)
import System.Posix.Types (Fd (Fd))

newtype EventFd = EventFd (ForeignPtr Fd)

eventFdToFd :: EventFd -> IO Fd
eventFdToFd (EventFd frnPtr) =
  withForeignPtr frnPtr $ pure . fromIntegral . ptrToIntPtr

data EventFdFlags = CloseOnExec | NonBlocking | Semaphore

eventFdFlagsToCInt :: EventFdFlags -> CInt
eventFdFlagsToCInt flag =
  case flag of
    CloseOnExec -> c_EFD_CLOEXEC
    NonBlocking -> c_EFD_NONBLOCK
    Semaphore -> c_EFD_SEMAPHORE

eventFd ::
  Foldable f => Word -> f EventFdFlags -> IO (Either IOException EventFd)
eventFd initVal flags = do
  evfd <-
    c_eventfd
      (fromIntegral initVal)
      (foldl' (\acc cur -> eventFdFlagsToCInt cur .|. acc) 0 flags)
  if evfd < 1
    then do
      n <- getErrno
      pure . Left $ errnoToIOError "eventFd" n Nothing Nothing
    else
      Right . EventFd
        <$> newForeignPtr c_eventfd_close (intPtrToPtr . IntPtr . fromIntegral $ evfd)

eventFd' ::
  Foldable f => Word -> f EventFdFlags -> ExceptT IOException IO EventFd
eventFd' initVal =
  ExceptT . eventFd initVal

-- | The same as eventFd, but can throw an IOException.
eventFd_ :: Foldable f => Word -> f EventFdFlags -> IO EventFd
eventFd_ initVal =
  either throwIO pure <=< eventFd initVal

write :: EventFd -> Int64 -> IO (Either IOException ())
write eventfd word = do
  Fd evfd <- eventFdToFd eventfd
  with word $ \ptr -> do
    val <- c_write evfd (castPtr ptr) 8
    if val < 0
      then do
        n <- getErrno
        pure . Left $ errnoToIOError "EventFd.write" n Nothing Nothing
      else pure $ Right ()

write' :: EventFd -> Int64 -> ExceptT IOException IO ()
write' eventfd =
  ExceptT . System.EventFd.write eventfd

-- | The same as 'System.EventFd' but can throw an IOException.
write_ :: EventFd -> Int64 -> IO ()
write_ eventfd =
  either throwIO pure <=< System.EventFd.write eventfd

read :: EventFd -> IO (Either IOException Int64)
read eventfd = do
  Fd evfd <- eventFdToFd eventfd
  allocaArray 8 $ \ptr -> do
    val <- c_read evfd ptr 8
    if val < 0
      then do
        n <- getErrno
        pure . Left $ errnoToIOError "EventFd.read" n Nothing Nothing
      else do
        Right <$> peek (castPtr ptr :: Ptr Int64)

read' :: EventFd -> ExceptT IOException IO Int64
read' =
  ExceptT . System.EventFd.read

read_ :: EventFd -> IO Int64
read_ =
  either throwIO pure <=< System.EventFd.read

close :: EventFd -> IO ()
close (EventFd frnPtr) = finalizeForeignPtr frnPtr
