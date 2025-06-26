{-# LANGUAGE OverloadedStrings #-}

module Home.Main (main) where

import Control.Concurrent (forkIO, myThreadId)
import Control.Exception (
  Exception (displayException),
  SomeException (..),
  bracket,
  bracket_,
  catch
 )
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecodeFileStrict)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Devices (Device (..))
import EventLoop (
  EventLoop,
  EventLoopT,
  MonadEventLoop (..),
  addMsg,
  addMsgIO,
  getLoop,
  runEventLoopT,
  setInterval,
 )
import Home.Env (Env, cameraServerSocket, cleanupEnv, mkEnv, router)
import Home.Handler (
  EstablishTLSConnection (..),
  ExHomeHandler (..),
  homeHandler,
 )
import Home.Options (
  HomeConfiguration,
  HomeOptions,
  configPath,
  proxyPort,
  proxyURL,
  tlsCACertificatePath,
  tlsCertificatePath,
  tlsKeyPath,
 )
import Lens.Micro
import Network.Socket (HostName, ServiceName, Socket)
import Options (runCommand)
import Proto.DeviceData qualified as Proto
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import Router (Router, connectionsRegistry, handleBytes)
import RxTx.Connection (Connection (..), cleanup)
import RxTx.Connection.Socket (getNewClientConnection)
import RxTx.ConnectionRegistry (addConnection, removeConnection)
import RxTx.Socket (SocketRxError (..))
import TLSHelper (setupTLSClientParams)

startCheckMemoryPoll ::
  EventLoopT Env (Device, ExHomeHandler) IO ()
startCheckMemoryPoll = do
  addMsg (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage))
  void $
    setInterval (Home, ExHomeHandler (defMessage @Proto.CheckMemoryUsage)) $
      30 * 1000

connectToProxyTLS ::
  HostName
  -> ServiceName
  -> FilePath
  -> FilePath
  -> FilePath
  -> EventLoopT Env (Device, ExHomeHandler) IO Bool
connectToProxyTLS host port certPath keyPath caCertPath = do
  mParams <-
    liftIO $
      setupTLSClientParams
        certPath
        keyPath
        caCertPath
        "raspberrypi"
  case mParams of
    Nothing -> do
      liftIO $ putStrLn "Failed to unpack TLS server params"
      pure False
    Just params -> do
      addMsg
        ( Home
        , ExHomeHandler $ EstablishTLSConnection params (T.pack host) (T.pack port)
        )
      pure True

listeningSocketThread ::
  EventLoop (Device, ExHomeHandler)
  -> Router
  -- -> ServerParams
  -> Socket
  -> IO ()
listeningSocketThread loop rtr listenSock =
  forever $ do
    bracket (getNewClientConnection listenSock) cleanup $ \conn -> do
      putStrLn $ "Accepted TCP connection from CAMERA"
      threadId <- myThreadId
      bracket_
        (addConnection Camera threadId conn (rtr ^. connectionsRegistry))
        (removeConnection Camera (rtr ^. connectionsRegistry))
        (runConnection conn)
 where
  runConnection conn = do
    merror <- recvAndDispatch conn
    case merror of
      Left (SocketRxError _ err) -> do
        print $ "ERROR: " <> show err
        cleanup conn
      Right bytes -> do
        handleBytes @Proto.WrappedEnvelope bytes rtr $ \src wrappedEnv ->
          case wrappedEnv ^. Proto.maybe'wrappedPayload of
            Just (Proto.WrappedEnvelope'HomeMsg proxyMsg) -> addMsgIO (src, ExHomeHandler proxyMsg) loop
            _ -> putStrLn "Dropping message for wrong device..."
        runConnection conn

mainImpl :: IO ()
mainImpl = runCommand $ \(opts :: HomeOptions) _args -> do
  mConfig <- eitherDecodeFileStrict (opts ^. configPath)
  case mConfig of
    Left errs -> putStrLn $ "Could not parse configuration file: " <> errs
    Right config -> do
      bracket mkEnv cleanupEnv $ \env -> do
        runEventLoopT (action config) env
        writeFile "/mnt/normalexit" ""
 where
  action ::
    HomeConfiguration -> EventLoopT Env (Device, ExHomeHandler) IO ()
  action config = do
    loop <- getLoop
    env <- getEnv
    _cameraSocketThread <-
      liftIO . forkIO $
        listeningSocketThread loop (env ^. router) (env ^. cameraServerSocket)
    success <-
      connectToProxyTLS
        "127.0.0.1"
        "3000"
        -- (config ^. proxyURL)
        -- (show $ config ^. proxyPort)
        (config ^. tlsCertificatePath)
        (config ^. tlsKeyPath)
        (config ^. tlsCACertificatePath)
    when success $ do
      startCheckMemoryPoll
      start $ uncurry homeHandler

main :: IO ()
main =
  forever $
    catch mainImpl $
      \(SomeException e) -> do
        putStrLn $ "An exception occurred: " <> displayException e
