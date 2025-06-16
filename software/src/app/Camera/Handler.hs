{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Handler (

) where

import Camera.Env (Env, videostreamRes, router)
import Camera.VideoStream (
  cleanupVideoStreamResource,
  createVideoStreamResource,
  getInitChunk,
  getStreamHandle, getChunk,
 )
import Control.Exception (bracketOnError)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (readIORef, writeIORef)
import Data.ProtoLens (defMessage)
import Devices (Device (Camera, Proxy))
import Envelope
import EventLoop (EventLoopT, MonadEventLoop (addMsg), getEnv)
import Lens.Micro ((&), (.~), (^.))
import Proto.Camera qualified as Proto
import Proto.Camera_Fields qualified as Proto
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import Router (trySendMessage)
import TH (makeInstance)

class CameraHandler msg where
  cameraHandler ::
    Device -> msg -> EventLoopT Env (Device, ExCameraHandler) IO ()

data ExCameraHandler = forall a. CameraHandler a => ExCameraHandler a

instance CameraHandler ExCameraHandler where
  cameraHandler island (ExCameraHandler msg) = cameraHandler island msg

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

-- -- Make CameraHandler Proto.ProxyRecieveEnvelope instance.
$( makeInstance
    ''CameraHandler
    ''Proto.CameraEnvelope
    'Proto.maybe'payload
    ''Proto.CameraEnvelope'Payload
 )

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
