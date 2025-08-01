module System.EventFd (
  EventFd,
  EventFdFlags (..),
  eventFdToFd,
  eventFd,
  System.EventFd.read,
  write,
  close,
) where

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
import Foreign.C (Errno, getErrno)
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

eventFd :: Foldable f => Word -> f EventFdFlags -> IO (Either Errno EventFd)
eventFd initVal flags = do
  evfd <-
    c_eventfd
      (fromIntegral initVal)
      (foldl' (\acc cur -> eventFdFlagsToCInt cur .|. acc) 0 flags)
  if evfd < 1
    then Left <$> getErrno
    else
      Right . EventFd
        <$> newForeignPtr c_eventfd_close (intPtrToPtr . IntPtr . fromIntegral $ evfd)

write :: EventFd -> Int64 -> IO (Either Errno ())
write eventfd word = do
  Fd evfd <- eventFdToFd eventfd
  with word $ \ptr -> do
    val <- c_write evfd (castPtr ptr) 8
    if val < 0 then Left <$> getErrno else pure $ Right ()

read :: EventFd -> IO (Either Errno Int64)
read eventfd = do
  Fd evfd <- eventFdToFd eventfd
  allocaArray 8 $ \ptr -> do
    val <- c_read evfd ptr 8
    if val < 0
      then Left <$> getErrno
      else do
        Right <$> peek (castPtr ptr :: Ptr Int64)

close :: EventFd -> IO ()
close (EventFd frnPtr) = finalizeForeignPtr frnPtr
