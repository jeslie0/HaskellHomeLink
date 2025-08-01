{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Handler where

import Camera.Env (Env, initChunk, router, videoStreamThread)
import Camera.VideoStream (
  VideoMessage (..),
  cleanupVideoStreamResource,
  createVideoStreamResource,
  getChunk,
  getInitChunk,
  getStreamHandle,
  startVideoStream,
 )
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracketOnError)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as B
import Data.Foldable (traverse_)
import Data.IORef (readIORef, writeIORef)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Time.Clock
import Data.Time.Format
import Devices (Device (..), proxies)
import Envelope
import EventLoop (
  EventLoopT,
  MonadEventLoop (addMsg),
  addMsgIO,
  getEnv,
  getLoop,
  setTimeoutIO,
 )
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Logger (LogLevel (..), reportLog)
import Proto.Camera qualified as Proto
import Proto.Camera_Fields qualified as Proto
import Proto.DeviceData qualified as Proto
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import ProtoHelper (ToMessage (..))
import Router (connectionsRegistry, handleBytes, trySendMessage)
import RxTx.Connection (Connection (..))
import RxTx.Connection.Socket (createClientConnection)
import RxTx.ConnectionRegistry (addConnection)
import RxTx.Socket (SocketRxError (..))
import System.Memory (getMemoryInformation)
import TH (makeInstance)

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

instance CameraHandler VideoMessage where
  cameraHandler _ (InitMsg bytes) = do
    env <- getEnv
    let msg = defMessage @Proto.InitialStreamMetaDataChunk & Proto.metadata .~ bytes
    liftIO . void $ trySendMessage (env ^. router) Proxy (wrapProxyMsg msg)
    liftIO . putStrLn $
      "Sent init chunk of size " <> show (B.length bytes) <> " to Proxy"
  cameraHandler _ (ChunkMsg bytes) = do
    env <- getEnv
    let msg = defMessage @Proto.StreamChunk & Proto.chunk .~ bytes
    liftIO . void $ trySendMessage (env ^. router) Proxy (wrapProxyMsg msg)
    now <- liftIO getCurrentTime
    liftIO $
      print $
        formatTime defaultTimeLocale "%H:%M:%S" now <> " Sending chunk to Proxy"

-- * Message instances

-- | Start the video stream
instance CameraHandler Proto.StartVideoStreamCmd where
  cameraHandler _ msg = do
    env <- getEnv
    loop <- getLoop
    mThread <- liftIO $ readIORef (env ^. videoStreamThread)
    case mThread of
      Just _ -> pure () -- Do nothing if stream in progress
      Nothing -> do
        thread <-
          liftIO . forkIO $
            startVideoStream
              (\msg -> void $ addMsgIO (Camera, ExCameraHandler msg) loop)
              (msg ^. Proto.raspividOpts)
              (msg ^. Proto.ffmpegOpts)
        liftIO $ writeIORef (env ^. videoStreamThread) (Just thread)

-- | Stop the video stream
instance CameraHandler Proto.StopVideoStreamCmd where
  cameraHandler _ _ = do
    env <- getEnv
    liftIO $ do
      videoStream <- readIORef (env ^. videoStreamThread)
      forM_ videoStream killThread
      writeIORef (env ^. videoStreamThread) Nothing

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
