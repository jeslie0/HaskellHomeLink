{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Handler where

import Camera.Env (Env, router, videostreamRes)
import Camera.VideoStream (
  cleanupVideoStreamResource,
  createVideoStreamResource,
  getChunk,
  getInitChunk,
  getStreamHandle,
 )
import Control.Concurrent (forkIO)
import Control.Exception (bracketOnError)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (traverse_)
import Data.IORef (readIORef, writeIORef)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Devices (Device (..), proxies)
import Envelope
import EventLoop (
  EventLoopT,
  MonadEventLoop (addMsg),
  addMsgIO,
  getEnv,
  setTimeoutIO,
  getLoop
 )
import Lens.Micro ((&), (.~), (^.))
import Logger (LogLevel (..), reportLog)
import Proto.Camera qualified as Proto
import Proto.Camera_Fields qualified as Proto
import Proto.DeviceData qualified as Proto
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import ProtoHelper (ToMessage (..))
import Router (handleBytes, trySendMessage, connectionsRegistry)
import RxTx.Connection (Connection (..))
import RxTx.Connection.Socket (createClientConnection)
import RxTx.ConnectionRegistry (addConnection)
import RxTx.Socket (SocketRxError (..))
import System.Memory (getMemoryInformation)
import TH (makeInstance)
import Lens.Micro.TH (makeLenses)

class CameraHandler msg where
  cameraHandler ::
    Device -> msg -> EventLoopT Env (Device, ExCameraHandler) IO ()

data ExCameraHandler = forall a. CameraHandler a => ExCameraHandler a

instance CameraHandler ExCameraHandler where
  cameraHandler island (ExCameraHandler msg) = cameraHandler island msg

-- -- Make CameraHandler Proto.ProxyRecieveEnvelope instance.
$( makeInstance
    ''CameraHandler
    ''Proto.CameraEnvelope
    'Proto.maybe'payload
    ''Proto.CameraEnvelope'Payload
 )

-- * Local messages

data SendInitChunk = SendInitChunk

instance CameraHandler SendInitChunk where
  cameraHandler _ _ = do
    env <- getEnv
    videoStream <- liftIO $ readIORef (env ^. videostreamRes)
    case videoStream of
      Nothing -> pure ()
      Just stream -> do
        let Just handle = getStreamHandle stream
        bytes <- liftIO $ getInitChunk handle
        let msg = defMessage @Proto.InitialStreamMetaDataChunk & Proto.metadata .~ bytes
        liftIO . void $ trySendMessage (env ^. router) Proxy (wrapProxyMsg msg)
        addMsg (Camera, ExCameraHandler SendChunk)

data SendChunk = SendChunk

instance CameraHandler SendChunk where
  cameraHandler _ _ = do
    env <- getEnv
    videoStream <- liftIO $ readIORef (env ^. videostreamRes)
    case videoStream of
      Nothing -> pure ()
      Just stream -> do
        let Just handle = getStreamHandle stream
        bytes <- liftIO $ getChunk handle
        let msg = defMessage @Proto.StreamChunk & Proto.chunk .~ bytes
        liftIO . void $ trySendMessage (env ^. router) Proxy (wrapProxyMsg msg)
        addMsg (Camera, ExCameraHandler SendChunk)

-- * Message instances


-- | Start the video stream
instance CameraHandler Proto.StartVideoStreamCmd where
  cameraHandler _ _ = do
    env <- getEnv
    videoStream <- liftIO $ readIORef (env ^. videostreamRes)
    case videoStream of
      Just _ -> pure () -- Do nothing if stream in progress
      Nothing -> do
        liftIO $ bracketOnError createVideoStreamResource cleanupVideoStreamResource $ \stream -> do
          writeIORef (env ^. videostreamRes) (Just stream)
        addMsg (Camera, ExCameraHandler SendInitChunk)

-- | Stop the video stream
instance CameraHandler Proto.StopVideoStreamCmd where
  cameraHandler _ _ = do
    env <- getEnv
    liftIO $ do
      videoStream <- readIORef (env ^. videostreamRes)
      forM_ videoStream cleanupVideoStreamResource
      writeIORef (env ^. videostreamRes) Nothing

instance CameraHandler Proto.CheckMemoryUsage where
  cameraHandler _ _ = do
    env <- getEnv
    mMemInfo <- liftIO getMemoryInformation
    case mMemInfo of
      Nothing -> liftIO $ reportLog (env ^. router) Error "Failed to get memory info"
      Just memInfo -> do
        forM_ proxies $ \proxy -> do
          liftIO $ do
            void . trySendMessage (env ^. router) proxy $
              wrapProxyMsg $
                toMessage @Proto.MemoryInformation memInfo

data EstablishHomeConnection = EstablishHomeConnection
  { _hostname :: {-# UNPACK #-} !T.Text
  , _port :: {-# UNPACK #-} !Int
  }

$(makeLenses ''EstablishHomeConnection)

instance CameraHandler EstablishHomeConnection where
  cameraHandler _ msg = do
    env <- getEnv
    loop <- getLoop
    let registry = env ^. router . connectionsRegistry
    void . liftIO $
      bracketOnError
        (createClientConnection (T.unpack $ msg ^. hostname) (show $ msg ^. port))
        (traverse_ cleanup)
        (withMaybeConn env loop registry)
   where
    withMaybeConn _ loop _ Nothing = do
      putStrLn "Failed to connect to HOME. Trying again in 2s..."
      void $ setTimeoutIO (Camera, ExCameraHandler msg) 2000 loop
    withMaybeConn env loop registry (Just conn) = do
      putStrLn "Connected to HOME"
      threadId <- forkIO . void $ runConnection loop (env ^. router) conn
      addConnection Home threadId conn registry

    runConnection loop rtr conn = do
      merror <- recvAndDispatch conn
      case merror of
        Left (SocketRxError _ err) -> do
          print $ "ERROR: " <> show err
          cleanup conn
          void $ setTimeoutIO (Camera, ExCameraHandler msg) 2000 loop
        Right bytes -> do
          handleBytes @Proto.WrappedEnvelope bytes rtr $ \src wrappedEnv ->
            case wrappedEnv ^. Proto.maybe'wrappedPayload of
              Just (Proto.WrappedEnvelope'CameraMsg cameraMsg) -> addMsgIO (src, ExCameraHandler cameraMsg) loop
              _ -> putStrLn "Dropping message for wrong device..."
          runConnection loop rtr conn
