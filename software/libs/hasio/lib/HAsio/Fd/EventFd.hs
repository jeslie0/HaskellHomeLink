module HAsio.Fd.EventFd (
  EventFd (..),
  EventFdFlags (..),
  eventFd,
  eventFd',
  eventFd_,
  HAsio.Fd.EventFd.read,
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
import HAsio.Error.ErrorStack (ErrorStack, pushErrno)
import HAsio.Error.Syscalls qualified as ESys
import HAsio.Fd.EventFd.Internal (
  c_EFD_CLOEXEC,
  c_EFD_NONBLOCK,
  c_EFD_SEMAPHORE,
  c_eventfd,
 )
import HAsio.Fd.IsFd (IsFd (..))
import HAsio.Fd.Syscalls (readUnsafe, writeUnsafe)
import System.Posix.Types (Fd (Fd))

newtype EventFd = EventFd Fd

instance IsFd EventFd where
  toFd (EventFd fd) = fd

  fromFd = EventFd

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
  with word $ \ptr -> do
    eVal <- writeUnsafe eventfd (castPtr ptr) (8 :: Int)
    case eVal of
      Left errs -> pure $ Left errs
      Right _ ->
        pure $ Right ()

write' :: EventFd -> Int64 -> ExceptT ErrorStack IO ()
write' eventfd =
  ExceptT . HAsio.Fd.EventFd.write eventfd

-- | The same as 'HAsio.Fd.EventFd' but can throw an IOException.
write_ :: EventFd -> Int64 -> IO ()
write_ eventfd =
  either
    throwIO
    pure
    <=< HAsio.Fd.EventFd.write eventfd

read :: EventFd -> IO (Either ErrorStack Int64)
read eventfd = do
  allocaArray 8 $ \ptr -> do
    eVal <- readUnsafe eventfd ptr (8 :: Int)
    case eVal of
      Left errs -> pure $ Left errs
      Right _ -> Right <$> peek (castPtr ptr :: Ptr Int64)

read' :: EventFd -> ExceptT ErrorStack IO Int64
read' =
  ExceptT . HAsio.Fd.EventFd.read

read_ :: EventFd -> IO Int64
read_ =
  either
    throwIO
    pure
    <=< HAsio.Fd.EventFd.read
