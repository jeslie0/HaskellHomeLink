module HIO.Fd.EventFd (
  EventFd (..),
  EventFdFlags (..),
  eventFdToFd,
  eventFd,
  eventFd',
  eventFd_,
  HIO.Fd.EventFd.read,
  read',
  read_,
  write,
  write',
  write_,
) where

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Foldable (Foldable (foldl'))
import Foreign (
  Int64,
  Ptr,
  Storable (..),
  allocaArray,
  castPtr,
  (.|.),
 )
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (with)
import HIO.Error.ErrorStack (ErrorStack, pushErrno)
import HIO.Error.Syscall qualified as ESys
import HIO.Fd.EventFd.Internal (
  c_EFD_CLOEXEC,
  c_EFD_NONBLOCK,
  c_EFD_SEMAPHORE,
  c_eventfd,
 )
import System.Posix.Internals (c_read, c_write)
import System.Posix.Types (Fd (Fd))

newtype EventFd = EventFd Fd

eventFdToFd :: EventFd -> Fd
eventFdToFd (EventFd fd) =
  fd

data EventFdFlags = CloseOnExec | NonBlocking | Semaphore

eventFdFlagsToCInt :: EventFdFlags -> CInt
eventFdFlagsToCInt flag =
  case flag of
    CloseOnExec -> c_EFD_CLOEXEC
    NonBlocking -> c_EFD_NONBLOCK
    Semaphore -> c_EFD_SEMAPHORE

eventFd ::
  Foldable f => Word -> f EventFdFlags -> IO (Either ErrorStack EventFd)
eventFd initVal flags = do
  evfd <-
    c_eventfd
      (fromIntegral initVal)
      (foldl' (\acc cur -> eventFdFlagsToCInt cur .|. acc) 0 flags)
  if evfd < 1
    then do
      Left <$> pushErrno ESys.EventFd
    else
      pure . Right $ EventFd (Fd evfd)

eventFd' ::
  Foldable f => Word -> f EventFdFlags -> ExceptT ErrorStack IO EventFd
eventFd' initVal =
  ExceptT . eventFd initVal

-- | The same as eventFd, but can throw an IOException.
eventFd_ :: Foldable f => Word -> f EventFdFlags -> IO EventFd
eventFd_ initVal =
  either
    throwIO
    pure
    <=< eventFd initVal

write :: EventFd -> Int64 -> IO (Either ErrorStack ())
write eventfd word = do
  let Fd evfd = eventFdToFd eventfd
  with word $ \ptr -> do
    val <- c_write evfd (castPtr ptr) 8
    if val < 0
      then do
        Left <$> pushErrno ESys.Write
      else pure $ Right ()

write' :: EventFd -> Int64 -> ExceptT ErrorStack IO ()
write' eventfd =
  ExceptT . HIO.Fd.EventFd.write eventfd

-- | The same as 'HIO.Fd.EventFd' but can throw an IOException.
write_ :: EventFd -> Int64 -> IO ()
write_ eventfd =
  either
    throwIO
    pure
    <=< HIO.Fd.EventFd.write eventfd

read :: EventFd -> IO (Either ErrorStack Int64)
read eventfd = do
  let Fd evfd = eventFdToFd eventfd
  allocaArray 8 $ \ptr -> do
    val <- c_read evfd ptr 8
    if val < 0
      then do
        Left <$> pushErrno ESys.Read
      else do
        Right <$> peek (castPtr ptr :: Ptr Int64)

read' :: EventFd -> ExceptT ErrorStack IO Int64
read' =
  ExceptT . HIO.Fd.EventFd.read

read_ :: EventFd -> IO Int64
read_ =
  either
    throwIO
    pure
    <=< HIO.Fd.EventFd.read
