module Camera.Main where

import Camera.Env (Env, mkEnv, cleanupEnv)
import Camera.Handler (ExCameraHandler (..), CameraHandler (cameraHandler))
import Camera.Options(CameraOptions)
import Camera.VideoStream (getChunk, getInitChunk)
import Control.Monad (void)
import Data.ByteString qualified as B
import Data.ProtoLens (defMessage)
import Devices (Device (Camera))
import EventLoop (EventLoopT, MonadEventLoop (..), addMsg, runEventLoopT)
import Options (runCommand)
import Proto.DeviceData qualified as Proto
import System.IO (IOMode (ReadMode), withFile)
import Control.Exception (bracket)

startCheckMemoryPoll ::
  EventLoopT Env (Device, ExCameraHandler) IO ()
startCheckMemoryPoll = do
  addMsg (Camera, ExCameraHandler (defMessage @Proto.CheckMemoryUsage))
  void $
    setInterval (Camera, ExCameraHandler (defMessage @Proto.CheckMemoryUsage)) $
      30 * 1000

main :: IO ()
main = do --runCommand $ \(opts :: CameraOptions) _args -> do
  bracket mkEnv cleanupEnv $ \env -> do
    runEventLoopT (action) env
    where
      action :: EventLoopT Env (Device, ExCameraHandler) IO ()
      action = do
        startCheckMemoryPoll
        start $ uncurry cameraHandler
