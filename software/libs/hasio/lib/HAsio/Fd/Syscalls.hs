module HAsio.Fd.Syscalls where

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Word (Word8)
import Foreign (Ptr, castPtr)
import Foreign.C (
  Errno (..),
  withCString,
 )
import HAsio.Error.ErrorStack (ErrorStack, push, pushErrno, singleton)
import HAsio.Error.Syscalls qualified as ESys
import HAsio.Fd.IsFd (IsFd (..))
import HAsio.Foreign (
  Flag,
  c_close,
  c_close_unsafe,
  c_open,
  c_open_unsafe,
  c_read,
  c_read_unsafe,
  c_write,
  c_write_unsafe,
  flagsToCInt,
 )
import System.Posix (Fd (..))
import Prelude hiding (read)
import Foreign.C.Types (CInt)

isFdToCInt :: IsFd fd => fd -> CInt
isFdToCInt fd =
  case toFd fd of
    Fd n -> n

-- ** Open

type Mode = Int

openUnsafe ::
  Foldable f => FilePath -> f Flag -> Mode -> IO (Either ErrorStack Fd)
openUnsafe path flags mode =
  withCString path $ \cPath -> do
    n <- c_open_unsafe cPath (flagsToCInt flags) (fromIntegral mode)
    pure $
      if n < 0
        then Left $ ESys.Open `push` singleton (Errno n)
        else Right $ Fd n

openUnsafe' ::
  Foldable f => FilePath -> f Flag -> Mode -> ExceptT ErrorStack IO Fd
openUnsafe' path flags =
  ExceptT . openUnsafe path flags

openUnsafe_ :: Foldable f => FilePath -> f Flag -> Mode -> IO Fd
openUnsafe_ path flags =
  either
    throwIO
    pure
    <=< openUnsafe path flags

open :: Foldable f => FilePath -> f Flag -> Mode -> IO (Either ErrorStack Fd)
open path flags mode =
  withCString path $ \cPath -> do
    n <- c_open cPath (flagsToCInt flags) (fromIntegral mode)
    pure $
      if n < 0
        then Left $ ESys.Open `push` singleton (Errno n)
        else Right $ Fd n

open' :: Foldable f => FilePath -> f Flag -> Mode -> ExceptT ErrorStack IO Fd
open' path flags =
  ExceptT . open path flags

open_ :: Foldable f => FilePath -> f Flag -> Mode -> IO Fd
open_ path flags =
  either
    throwIO
    pure
    <=< open path flags

-- ** Read

readUnsafe ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> IO (Either ErrorStack n)
readUnsafe fd ptr len = do
  val <- c_read_unsafe (isFdToCInt fd) (castPtr ptr) (fromIntegral len)
  if val < 0
    then do
      Left <$> pushErrno ESys.Read
    else pure . Right $ fromIntegral val

readUnsafe' ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> ExceptT ErrorStack IO n
readUnsafe' fd ptr = do
  ExceptT . readUnsafe fd ptr

readUnsafe_ ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> IO n
readUnsafe_ fd ptr =
  either
    throwIO
    pure
    <=< readUnsafe fd ptr

read ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> IO (Either ErrorStack n)
read fd ptr len = do
  val <- c_read (isFdToCInt fd) (castPtr ptr) (fromIntegral len)
  if val < 0
    then do
      Left <$> pushErrno ESys.Read
    else pure . Right $ fromIntegral val

read' ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> ExceptT ErrorStack IO n
read' fd ptr = do
  ExceptT . read fd ptr

read_ ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> IO n
read_ fd ptr =
  either
    throwIO
    pure
    <=< read fd ptr

-- ** Write

writeUnsafe ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> IO (Either ErrorStack n)
writeUnsafe fd ptr len = do
  val <- c_write_unsafe (isFdToCInt fd) (castPtr ptr) (fromIntegral len)
  if val < 0
    then do
      Left <$> pushErrno ESys.Write
    else pure . Right $ fromIntegral val

writeUnsafe' ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> ExceptT ErrorStack IO n
writeUnsafe' fd ptr =
  ExceptT . writeUnsafe fd ptr

writeUnsafe_ ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> IO n
writeUnsafe_ fd ptr =
  either
    throwIO
    pure
    <=< writeUnsafe fd ptr

write ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> IO (Either ErrorStack n)
write fd ptr len = do
  val <- c_write (isFdToCInt fd) (castPtr ptr) (fromIntegral len)
  if val < 0
    then do
      Left <$> pushErrno ESys.Write
    else pure . Right $ fromIntegral val

write' ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> ExceptT ErrorStack IO n
write' fd ptr =
  ExceptT . write fd ptr

write_ ::
  (Integral n, IsFd fd) => fd -> Ptr Word8 -> n -> IO n
write_ fd ptr =
  either
    throwIO
    pure
    <=< write fd ptr

-- ** Close

closeUnsafe :: IsFd fd => fd -> IO (Either ErrorStack ())
closeUnsafe fd = do
  val <- c_close_unsafe (isFdToCInt fd)
  if val < 0
    then do
      Left <$> pushErrno ESys.Close
    else pure . Right $ ()

closeUnsafe' :: IsFd fd => fd -> ExceptT ErrorStack IO ()
closeUnsafe' =
  ExceptT . closeUnsafe

closeUnsafe_ :: IsFd fd => fd -> IO ()
closeUnsafe_ =
  either
    throwIO
    pure
    <=< closeUnsafe

close :: IsFd fd => fd -> IO (Either ErrorStack ())
close fd = do
  val <- c_close (isFdToCInt fd)
  if val < 0
    then do
      Left <$> pushErrno ESys.Close
    else pure . Right $ ()

close' :: IsFd fd => fd -> ExceptT ErrorStack IO ()
close' =
  ExceptT . close

close_ :: IsFd fd => fd -> IO ()
close_ =
  either
    throwIO
    pure
    <=< close
