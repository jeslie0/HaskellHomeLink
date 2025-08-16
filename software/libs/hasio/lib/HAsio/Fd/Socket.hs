module HAsio.Fd.Socket where

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Word (Word8)
import Foreign (Bits ((.|.)), Ptr, castPtr)
import Foreign.C.Types (CInt)
import HAsio.Error.ErrorStack (ErrorStack, pushErrno)
import HAsio.Error.Syscalls qualified as ESys
import HAsio.Fd.IsFd (IsFd (..))
import HAsio.Fd.Socket.Internal (
  c_MSG_CONFIRM,
  c_MSG_DONTROUTE,
  c_MSG_DONTWAIT,
  c_MSG_EOR,
  c_MSG_FASTOPEN,
  c_MSG_MORE,
  c_MSG_NOSIGNAL,
  c_MSG_OOB,
  c_send,
  c_send_unsafe,
 )
import System.Posix (Fd (..))

newtype Socket = Socket Fd deriving (Eq, Show)

instance IsFd Socket where
  toFd (Socket fd) = fd

  fromFd = Socket

data SendFlag
  = MsgConfirm
  | MsgDontRoute
  | MsgDontWait
  | MsgEor
  | MsgMore
  | MsgNoSignal
  | MsgOOB
  | MsgFastOpen
  deriving (Eq)

sendFlagToCInt :: SendFlag -> CInt
sendFlagToCInt flag =
  case flag of
    MsgConfirm -> c_MSG_CONFIRM
    MsgDontRoute -> c_MSG_DONTROUTE
    MsgDontWait -> c_MSG_DONTWAIT
    MsgEor -> c_MSG_EOR
    MsgMore -> c_MSG_MORE
    MsgNoSignal -> c_MSG_NOSIGNAL
    MsgOOB -> c_MSG_OOB
    MsgFastOpen -> c_MSG_FASTOPEN

combineFlags :: Foldable f => f SendFlag -> CInt
combineFlags =
  foldl (\cur prev -> cur .|. sendFlagToCInt prev) 0

sendUnsafe ::
  Integral n => Socket -> Ptr Word8 -> n -> [SendFlag] -> IO (Either ErrorStack n)
sendUnsafe (Socket (Fd fd)) ptr len flags = do
  n <- c_send_unsafe fd (castPtr ptr) (fromIntegral len) (combineFlags flags)
  if n < 0
    then
      Left <$> pushErrno ESys.Send
    else pure . Right $ fromIntegral n

sendUnsafe' ::
  Integral n => Socket -> Ptr Word8 -> n -> [SendFlag] -> ExceptT ErrorStack IO n
sendUnsafe' sock ptr len =
  ExceptT . sendUnsafe sock ptr len

sendUnsafe_ ::
  Integral n => Socket -> Ptr Word8 -> n -> [SendFlag] -> IO n
sendUnsafe_ sock ptr len =
  either throwIO pure <=< sendUnsafe sock ptr len

send ::
  Integral n => Socket -> Ptr Word8 -> n -> [SendFlag] -> IO (Either ErrorStack n)
send (Socket (Fd fd)) ptr len flags = do
  n <- c_send fd (castPtr ptr) (fromIntegral len) (combineFlags flags)
  if n < 0
    then
      Left <$> pushErrno ESys.Send
    else pure . Right $ fromIntegral n

send' ::
  Integral n => Socket -> Ptr Word8 -> n -> [SendFlag] -> ExceptT ErrorStack IO n
send' sock ptr len =
  ExceptT . send sock ptr len

send_ ::
  Integral n => Socket -> Ptr Word8 -> n -> [SendFlag] -> IO n
send_ sock ptr len =
  either throwIO pure <=< send sock ptr len
