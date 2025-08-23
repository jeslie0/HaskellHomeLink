{-# LANGUAGE LambdaCase #-}

module Test where

import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import HAsio.Async.IO (asyncSendAll, asyncAccept)
import HAsio.Async.Reactor (Reactor, cancel, run, withReactor, registerFd, deregisterFd)
import HAsio.Fd (IsFd (..))
import HAsio.Fd.Socket (Socket, ownNetworkSocket)
import HAsio.Fd.Syscalls (closeUnsafe_)
import Network.Socket (
  AddrInfo (..),
  ServiceName,
  SocketType (Stream),
  close,
  defaultHints,
  getAddrInfo,
  openSocket,
  socket,
  socketToFd,
  unsafeFdSocket,
 )
import Network.Socket qualified as Network
import Network.Socket.Address (connect)
import RxTx.Connection.Socket (mkServerAddrInfo)
import System.Posix (Fd (..), FdOption (NonBlockingRead), setFdOption)
import System.Posix.Internals (setNonBlockingFD)
import HAsio.Fd.Epoll (Event(..))
import HAsio.Fd.Epoll (Flag(..))
import HAsio.Async.IO (asyncRecv)
import HAsio.Async.IO (RecvResult(..))

aquireBoundListeningServerSocket ::
  ServiceName
  -> IO Socket
aquireBoundListeningServerSocket port = do
  addrInfo <- mkServerAddrInfo port
  sock <-
    socket (addrFamily addrInfo) (addrSocketType addrInfo) (addrProtocol addrInfo)
  Network.setSocketOption sock Network.ReuseAddr 1
  Network.bind sock (addrAddress addrInfo)
  Network.listen sock 1
  putStrLn $ "Listening on port " <> port <> " for TLS connections..."
  ownNetworkSocket sock

main :: IO ()
main = do
  bracket (aquireBoundListeningServerSocket "3000") closeUnsafe_ $ \listenSock -> do
    setFdOption (toFd listenSock) NonBlockingRead True
    val <- runExceptT . withReactor @Reactor $ go listenSock
    case val of
      Left errs -> print errs
      Right _ -> pure ()
 where
  go listenSock reactor = do
    asyncAccept reactor listenSock $ \case
      Left errs -> liftIO (print errs) >> cancel reactor
      Right connected -> do
        liftIO $ setFdOption (toFd connected) NonBlockingRead True
        asyncRecv reactor connected $ \case
          Left errs -> liftIO (print errs) >>liftIO (print errs) >> cancel reactor
          Right RecvClosed -> deregisterFd reactor connected EpollIn
          Right (RecvData bytes) -> liftIO $ print bytes
        pure ()
    run reactor

main' :: IO ()
main' = do
  val <- runExceptT go
  case val of
    Left errs -> print errs
    Right val -> pure ()
 where
  resolve = do
    let hints = defaultHints {addrSocketType = Stream}
    head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "9000")

  open addr = bracketOnError (openSocket addr) close $ \sock -> do
    connect sock $ addrAddress addr
    return sock

  go = withReactor @Reactor $ \reactor -> do
    addr <- liftIO resolve
    liftIO $ bracket (open addr) close $ \netSock -> do
      cint <- socketToFd netSock
      setFdOption (Fd cint) NonBlockingRead True
      result <- runExceptT . forever $ woof reactor (fromFd $ Fd cint)
      case result of
        Left errs -> print errs
        Right _ -> pure ()

  woof reactor sock = do
    liftIO $ print "woof"
    str <- liftIO B.getLine
    asyncSendAll
      reactor
      sock
      str
      ( \case
          Left errs -> liftIO (print errs) >> cancel reactor
          Right _ -> liftIO (putStrLn "Done!") >> cancel reactor
      )
    run reactor
