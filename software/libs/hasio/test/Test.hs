{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (
  Exception (..),
  SomeException,
  bracket,
  bracketOnError,
  catch,
 )
import Control.Monad (forM_, forever, void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Default.Class (def)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.X509 (Certificate, CertificateChain (CertificateChain), SignedExact)
import Data.X509.CertificateStore (
  CertificateStore,
  listCertificates,
  makeCertificateStore,
 )
import Data.X509.File (readKeyFile, readSignedObject)
import HAsio.Async.EventLoop (
  addMsg,
  cancel,
  getReactor,
  run,
  setTimeout,
  withEventLoop,
 )
import HAsio.Async.IO (
  RecvResult (..),
  asyncAccept,
  asyncRead,
  asyncRecv,
  asyncSendAll,
 )
import HAsio.Async.Reactor (
  Reactor,
  deregisterFd,
  registerFd,
  withReactor,
 )
import HAsio.Async.TLS (
  TLSConnection,
  handShakeTLSConnection,
  mkTLSConnection,
  registerTLSConnection,
  sendData,
  setBlocking,
  setNonBlocking,
 )
import HAsio.Error.ErrorStack
import HAsio.Fd (IsFd (..), stdInput)
import HAsio.Fd.Epoll (Event (..), Flag (..))
import HAsio.Fd.Socket (Socket (..), ownNetworkSocket)
import HAsio.Fd.Syscalls (close', closeUnsafe_, close_)
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
import Network.TLS qualified as TLS
import Network.TLS.Extra qualified as TLS
import RxTx.Connection.Socket hiding (aquireClientSocket, mkAddrInfo)
import System.Posix (Fd (..), FdOption (NonBlockingRead), setFdOption)
import System.Posix.Internals (setNonBlockingFD)

loadCAStore :: FilePath -> IO CertificateStore
loadCAStore caCertPath = do
  certs <- readSignedObject caCertPath
  pure $ makeCertificateStore certs

loadCredentials :: FilePath -> FilePath -> IO (Maybe TLS.Credential)
loadCredentials certPath keyPath = do
  certs :: [SignedExact Certificate] <- readSignedObject certPath
  keys <- readKeyFile keyPath
  case keys of
    [key] -> pure . Just $ (CertificateChain certs, key)
    _ -> pure Nothing

setupTLSServerParams ::
  TLS.HostName
  -> FilePath
  -- ^ Certificate path
  -> FilePath
  -- ^ Key path
  -> IO (Maybe TLS.ServerParams)
setupTLSServerParams hostname certPath keyPath = do
  mCreds <- loadCredentials certPath keyPath
  case mCreds of
    Nothing -> pure Nothing
    Just creds ->
      pure . Just $
        def
          { TLS.serverWantClientCert = False
          , TLS.serverShared = def {TLS.sharedCredentials = TLS.Credentials [creds]}
          -- , TLS.serverSupported =
          --     def
          --       { supportedCiphers = ciphersuite_strong
          --       , supportedVersions = [TLS13]
          --       }
          -- , TLS.serverHooks = mTLSHooks hostname caStore
          }

setupTLSClientParams ::
  FilePath
  -- ^ CA Certificate path
  -> TLS.HostName
  -> IO (Maybe TLS.ClientParams)
setupTLSClientParams caCertPath hostname = do
  caStore <- loadCAStore caCertPath
  let defaultParams = TLS.defaultParamsClient hostname ""
  pure . Just $
    defaultParams
      { --   TLS.clientSupported =
        --     def
        --       { TLS.supportedCiphers = TLS.ciphersuite_strong
        --       , TLS.supportedVersions = [TLS.TLS13]
        --       }
        TLS.clientShared =
          def
            { -- TLS.sharedCredentials = TLS.Credentials [creds]
              TLS.sharedCAStore = caStore
            }
            -- TLS.clientUseServerNameIndication = False
            -- , TLS.clientHooks =
            --     def
            --       { TLS.onServerCertificate = \_ ->
            --           validateDefault caStore
            --       , TLS.onCertificateRequest = \_ -> pure $ Just creds
            --       }
      }

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

-- TODO getAddrInfo can throw an IOError.
mkAddrInfo :: TLS.HostName -> ServiceName -> ExceptT ErrorStack IO AddrInfo
mkAddrInfo host port = do
  let hints = defaultHints {addrSocketType = Stream}
  results <- liftIO $ getAddrInfo (Just hints) (Just host) (Just port)
  case results of
    (addr : _) -> pure addr
    [] -> undefined -- makeErrorStack FailedToExtractAddrInfo

aquireClientSocket ::
  TLS.HostName -> ServiceName -> ExceptT ErrorStack IO (Socket, AddrInfo)
aquireClientSocket host port = do
  addrInfo <- mkAddrInfo host port
  sock <- liftIO $ openSocket addrInfo
  liftIO $ Network.connect sock $ addrAddress addrInfo
  fdSock <- liftIO $ ownNetworkSocket sock
  pure (fdSock, addrInfo)

-- main'' :: IO ()
-- main'' = do
--   bracket (aquireBoundListeningServerSocket "3000") closeUnsafe_ $ \listenSock -> do
--     setFdOption (toFd listenSock) NonBlockingRead True
--     val <- runExceptT . withReactor @Reactor $ go listenSock
--     case val of
--       Left errs -> print errs
--       Right _ -> pure ()
--  where
--   go listenSock reactor = do
--     sockRef <- liftIO $ newIORef Nothing
--     asyncAccept reactor listenSock $ \case
--       Left errs -> do
--         liftIO (print errs)
--         deregisterFd reactor listenSock EpollIn
--         close' listenSock
--         cancel reactor
--       Right connected -> do
--         liftIO $ writeIORef sockRef (Just connected)
--         liftIO $ setFdOption (toFd connected) NonBlockingRead True
--         unregister <- asyncRecv reactor connected $ \case
--           Left errs -> do
--             liftIO (print errs)
--             liftIO $ writeIORef sockRef Nothing
--             deregisterFd reactor connected EpollIn
--             close' connected
--             cancel reactor
--           Right RecvClosed -> do
--             deregisterFd reactor connected EpollIn
--             close' connected
--             liftIO $ writeIORef sockRef Nothing
--           Right (RecvData bytes) -> liftIO $ print bytes
--         pure ()

--     liftIO $ setFdOption stdInput NonBlockingRead True
--     asyncRead reactor stdInput $ \case
--       Left errs -> do
--         liftIO (print errs)
--         deregisterFd reactor stdInput EpollIn
--         close' stdInput
--         cancel reactor
--       Right RecvClosed -> do
--         deregisterFd reactor stdInput EpollIn
--         close' stdInput
--         cancel reactor
--       Right (RecvData bytes) -> do
--         mSock <- liftIO $ readIORef sockRef
--         forM_ mSock $ \sock -> asyncSendAll reactor sock bytes $ \case
--           Right _ -> pure ()
--           Left errs -> do
--             liftIO (print errs)
--             deregisterFd reactor stdInput EpollIn
--             close' stdInput
--             cancel reactor

--     run reactor

-- main' :: IO ()
-- main' = do
--   val <- runExceptT go
--   case val of
--     Left errs -> print errs
--     Right val -> pure ()
--  where
--   resolve = do
--     let hints = defaultHints {addrSocketType = Stream}
--     head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "9000")

--   open addr = bracketOnError (openSocket addr) close $ \sock -> do
--     connect sock $ addrAddress addr
--     return sock

--   go = withReactor @Reactor $ \reactor -> do
--     addr <- liftIO resolve
--     liftIO $ bracket (open addr) close $ \netSock -> do
--       cint <- socketToFd netSock
--       setFdOption (Fd cint) NonBlockingRead True
--       result <- runExceptT . forever $ woof reactor (fromFd $ Fd cint)
--       case result of
--         Left errs -> print errs
--         Right _ -> pure ()

--   woof reactor sock = do
--     liftIO $ print "woof"
--     str <- liftIO B.getLine
--     asyncSendAll
--       reactor
--       sock
--       str
--       ( \case
--           Left errs -> liftIO (print errs) >> cancel reactor
--           Right _ -> liftIO (putStrLn "Done!") >> cancel reactor
--       )
--     run reactor

data Events = Connect TLS.ClientParams String | Ev1 TLSConnection | Ev2 | EvDelay

handleEvents loop (Connect params port) = do
  (connSock, _) <- aquireClientSocket "127.0.0.1" port
  conn <- mkTLSConnection params (toFd connSock)
  setBlocking conn
  handShakeTLSConnection conn
  setNonBlocking (getReactor loop) conn
  registerTLSConnection (getReactor loop) conn (liftIO . print)

  addMsg (Ev1 conn) loop
handleEvents _ (Ev1 conn) = do
  liftIO $ print "reading file"
  bytes <- liftIO $ B.readFile "/home/james/BIN"
  liftIO $ print $ "file is " <> show (B.length bytes) <> " bytes"
  sendData conn bytes
handleEvents _ Ev2 = liftIO . putStrLn $ "Ev2"
handleEvents loop EvDelay = do
  liftIO . putStrLn $ "Ev2"
  void $ setTimeout loop EvDelay 1000

tlsServer port withBytes = do
  Just params <-
    setupTLSServerParams
      "localhost"
      "/home/james/crypto/dev/Dev_User_Cert.crt"
      "/home/james/crypto/dev/Dev_User_Cert.pem"
  bracket (Test.aquireBoundListeningServerSocket port) close_ $ \listenSock -> do
    val <- runExceptT $ withEventLoop handleEvents $ \loop -> do
      registerFd (getReactor loop) stdInput EpollIn [EpollET] $ cancel loop
      asyncAccept (getReactor loop) listenSock $ \case
        Left errs -> liftIO $ print errs
        Right sock -> do
          conn <- mkTLSConnection params (toFd sock)
          setBlocking conn
          handShakeTLSConnection conn
          setNonBlocking (getReactor loop) conn
          registerTLSConnection (getReactor loop) conn withBytes
          pure ()

      run loop
    case val of
      Left errs -> print errs
      Right _ -> pure ()

tlsClient port = do
  Just params <-
    setupTLSClientParams
      "/home/james/crypto/dev/Dev_CA.crt"
      "127.0.0.1"
  val <- runExceptT $ withEventLoop handleEvents $ \loop -> do
    registerFd (getReactor loop) stdInput EpollIn [EpollET] $ cancel loop
    addMsg (Connect params port) loop

    run loop
  case val of
    Left errs -> print errs
    Right _ -> pure ()

main :: IO ()
main = do
  let port = "5005"
  forkIO $ do
    tlsServer port (liftIO . B.appendFile "output") `catch` \(e :: SomeException) -> do
      print "SERVER EXCEPTION"
      print $ displayException e
  threadDelay 10000
  tlsClient port `catch` \(e :: SomeException) -> do
    print "CLIENT EXCEPTION"
    print $ displayException e
