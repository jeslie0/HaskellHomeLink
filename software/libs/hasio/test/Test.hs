{-# LANGUAGE LambdaCase #-}

module Test where

import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import HAsio.Async.IO (asyncSendAll)
import HAsio.Async.Reactor ( Reactor, withReactor, cancel, run )
import HAsio.Fd (IsFd (..))
import Network.Socket (
  AddrInfo (..),
  SocketType (Stream),
  close,
  defaultHints,
  getAddrInfo,
  openSocket,
  unsafeFdSocket, socketToFd,
 )
import Network.Socket.Address (connect)
import System.Posix (Fd (..), setFdOption, FdOption (NonBlockingRead))
import System.Posix.Internals (setNonBlockingFD)

main :: IO ()
main = do
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
