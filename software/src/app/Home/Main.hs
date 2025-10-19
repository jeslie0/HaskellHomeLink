{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Home.Main (main) where

import Control.Exception (
  Exception (displayException),
  SomeException (..),
  catch,
 )
import Control.Monad (forever, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString qualified as B
import Data.ProtoLens (defMessage)
import Data.Serialize (putWord32be, runPut)
import Data.Text qualified as T
import Devices (Device (..))
import HAsio.Async.EventLoop (
  addMsg,
  cancel,
  getReactor,
  run,
  setInterval,
  withEventLoop,
 )
import HAsio.Async.IO (
  RecvResult (..),
  asyncAccept,
  asyncRecv,
  asyncRecvN,
  asyncSendAll,
 )
import HAsio.Async.Reactor (deregisterFd, registerFd)
import HAsio.Control (bracket, bracketOnError)
import HAsio.Fd (setNonBlocking)
import HAsio.Fd.Epoll (Event (..))
import HAsio.Fd.Socket (Socket)
import HAsio.Fd.Syscalls (close')
import Home.Env (Env, cleanupEnv, mkEnv)
import Home.Handler (
  EstablishTLSConnection (..),
  ExHomeHandler (..),
 )
import Home.Options (
  HomeConfiguration,
  HomeOptions,
  configPath,
 )
import Lens.Micro
import Network.Socket (HostName, ServiceName)
import Options (runCommand)
import Ports (homeCameraRecvPort)
import Proto.DeviceData qualified as Proto
import TCP (
  aquireBoundListeningServerSocket,
  aquireClientSocket,
  asyncRecvHandler,
 )
import TLSHelper (setupTLSClientParams)

-- startCheckMemoryPoll ::
--   EventLoopT Env (Device, ExHomeHandler) IO ()
-- startCheckMemoryPoll = do
--   addMsg (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
--   void $
--     setInterval (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $
--       30 * 1000

-- connectToProxyTLS ::
--   HostName
--   -> ServiceName
--   -> FilePath
--   -> FilePath
--   -> FilePath
--   -> EventLoopT Env (Device, ExHomeHandler) IO Bool
-- connectToProxyTLS host port certPath keyPath caCertPath = do
--   mParams <-
--     liftIO $
--       setupTLSClientParams
--         certPath
--         keyPath
--         caCertPath
--         "raspberrypi"
--   case mParams of
--     Nothing -> do
--       liftIO $ putStrLn "Failed to unpack TLS server params"
--       pure False
--     Just params -> do
--       addMsg
--         ( Home
--         , ExHomeHandler $ EstablishTLSConnection params (T.pack host) (T.pack port)
--         )
--       pure True

-- listeningSocketThread ::
--   EventLoop (Device, ExHomeHandler)
--   -> Router
--   -- -> ServerParams
--   -> Socket
--   ->  ()
-- listeningSocketThread loop rtr listenSock =
--   forever $ do
--     bracket (getNewClientConnection listenSock) cleanup $ \conn -> do
--       putStrLn $ "Accepted TCP connection from CAMERA"
--       threadId <- myThreadId
--       bracket_
--         (addConnection Camera threadId conn (rtr ^. connectionsRegistry))
--         (removeConnection Camera (rtr ^. connectionsRegistry))
--         (runConnection conn)
--  where
--   runConnection conn = do
--     merror <- recvAndDispatch conn
--     case merror of
--       Left (SocketRxError _ err) -> do
--         print $ "ERROR: " <> show err
--         cleanup conn
--       Right bytes -> do
--         handleBytes @Proto.WrappedEnvelope bytes rtr $ \src wrappedEnv ->
--           case wrappedEnv ^. Proto.maybe'wrappedPayload of
--             Just (Proto.WrappedEnvelope'HomeMsg proxyMsg) -> addMsgIO (src, ExHomeHandler proxyMsg) loop
--             _ -> putStrLn "Dropping message for wrong device..."
--         runConnection conn

data MsgType
  = CreateClientConn
  | SendMessage Socket

mainImpl :: IO ()
mainImpl = runCommand $ \(opts :: HomeOptions) _args -> do
  -- mConfig <- eitherDecodeFileStrict @HomeConfiguration (opts ^. configPath)
  -- case mConfig of
  --   Left errs -> putStrLn $ "Could not parse configuration file: " <> errs
  --   Right config -> do
  result <- runExceptT $ bracket mkEnv cleanupEnv $ \env -> do
    bracket (aquireBoundListeningServerSocket $ show homeCameraRecvPort) close' $ \cameraListenSock -> do
      liftIO $ setNonBlocking cameraListenSock
      withEventLoop handler $ \loop -> do
        void . asyncAccept (getReactor loop) cameraListenSock $ \case
          Left errs -> liftIO $ print errs
          Right socket -> do
            void $ asyncRecvHandler (getReactor loop) socket $ \bytes -> do
              liftIO $ print bytes
        addMsg CreateClientConn loop
        run loop
  case result of
    Left errs -> print errs
    Right _ -> pure ()
 where
  handler loop CreateClientConn = do
    bracketOnError
      (aquireClientSocket "127.0.0.1" "9000")
      (\(sock, _) -> close' sock)
      $ \(sock, addrInfo) -> do
        liftIO $ setNonBlocking sock
        void $ setInterval loop (SendMessage sock) 1000
  handler loop (SendMessage sock) = do
    let
      message = "hi"
      hdr = runPut . putWord32be . fromIntegral $ B.length message
    asyncSendAll (getReactor loop) sock (hdr <> message) $ \case
      Left errs -> liftIO $ print errs
      Right _ -> liftIO $ print "sent message"

-- runExcept $ bracket mkEnv cleanupEnv $ \env -> do
--   withEventLoop _ _
-- runEventLoopT (action config) env
-- writeFile "/mnt/normalexit" ""

-- action ::
--   HomeConfiguration -> EventLoopT Env (Device, ExHomeHandler) IO ()
-- action config = do
--   loop <- getLoop
--   env <- getEnv
--   _cameraSocketThread <-
--     liftIO . forkIO $
--       listeningSocketThread loop (env ^. router) (env ^. cameraServerSocket)
--   success <-
--     connectToProxyTLS
--       -- "127.0.0.1"
--       -- "3000"
--       (config ^. proxyURL)
--       (show $ config ^. proxyPort)
--       (config ^. tlsCertificatePath)
--       (config ^. tlsKeyPath)
--       (config ^. tlsCACertificatePath)
--   when success $ do
--     startCheckMemoryPoll
--     start $ uncurry homeHandler

main :: IO ()
main = mainImpl

-- forever $
--   catch mainImpl $
--     \(SomeException e) -> do
--       putStrLn $ "An exception occurred: " <> displayException e
