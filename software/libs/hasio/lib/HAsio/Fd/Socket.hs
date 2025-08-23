module HAsio.Fd.Socket where

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Word (Word8)
import Foreign (Bits ((.|.)), Ptr, castPtr)
import Foreign.C.Types (CInt)
import Foreign.Ptr (nullPtr)
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
  c_SOCK_CLOEXEC,
  c_SOCK_NONBLOCK,
  c_accept4_unsafe,
  c_recv_unsafe,
  c_send,
  c_send_unsafe,
 )
import Network.Socket qualified as Network
import System.Posix (Fd (..))

newtype Socket = Socket Fd deriving (Eq, Show)

ownNetworkSocket :: Network.Socket -> IO Socket
ownNetworkSocket sock = do
  n <- Network.unsafeFdSocket sock
  Network.touchSocket sock
  print n
  pure . Socket . Fd $ n

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

-- * recv

data RecvFlag

-- MsgDontWait
-- \| MsgErrQueue
-- \| MsgOOB
-- \| MsgPeek
-- \| MsgTrunc
-- \| MsgWaitAll

recvUnsafe ::
  Integral n => Socket -> Ptr Word8 -> n -> [RecvFlag] -> IO (Either ErrorStack n)
recvUnsafe (Socket (Fd fd)) ptr len _flags = do
  n <- c_recv_unsafe fd (castPtr ptr) (fromIntegral len) 0
  if n < 0
    then
      Left <$> pushErrno ESys.Recv
    else pure . Right $ fromIntegral n

recvUnsafe' ::
  Integral n => Socket -> Ptr Word8 -> n -> [RecvFlag] -> ExceptT ErrorStack IO n
recvUnsafe' sock ptr len = do
  ExceptT . recvUnsafe sock ptr len

recvUnsafe_ :: Integral n => Socket -> Ptr Word8 -> n -> [RecvFlag] -> IO n
recvUnsafe_ sock ptr len =
  either throwIO pure <=< recvUnsafe sock ptr len

data SocketFlag
  = SocketNonBlock
  | SocketCloExec

socketFlagToCInt :: SocketFlag -> CInt
socketFlagToCInt flag =
  case flag of
    SocketNonBlock -> c_SOCK_NONBLOCK
    SocketCloExec -> c_SOCK_CLOEXEC

acceptUnsafe :: Socket -> [SocketFlag] -> IO (Either ErrorStack Socket)
acceptUnsafe (Socket (Fd fd)) flags = do
  n <-
    c_accept4_unsafe
      fd
      nullPtr
      nullPtr
      (foldl' (\acc val -> socketFlagToCInt val .|. acc) 0 flags)
  if n < 0
    then
      Left <$> pushErrno ESys.Accept
    else pure . Right . Socket . Fd $ n

acceptUnsafe' :: Socket -> [SocketFlag] -> ExceptT ErrorStack IO Socket
acceptUnsafe' socket = do
  ExceptT . acceptUnsafe socket

acceptUnsafe_ :: Socket -> [SocketFlag] -> IO Socket
acceptUnsafe_ socket = do
  either throwIO pure <=< acceptUnsafe socket
