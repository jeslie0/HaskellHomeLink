{-# LANGUAGE OverloadedStrings #-}

module Camera.Main where

import Camera.Env (Env, cleanupEnv, mkEnv)
import Camera.Handler (
  CameraHandler (cameraHandler),
  EstablishHomeConnection (..),
  ExCameraHandler (..),
 )
import Camera.Options (CameraOptions)
import Camera.VideoStream (getChunk, getInitChunk)
import Control.Exception (
  Exception (displayException),
  SomeException (..),
  bracket,
  catch,
 )
import Control.Monad (forever, void)
import Data.ByteString qualified as B
import Data.ProtoLens (defMessage)
import Devices (Device (Camera))
import EventLoop (EventLoopT, MonadEventLoop (..), addMsg, runEventLoopT)
import Options (runCommand)
import Proto.DeviceData qualified as Proto
import System.IO (IOMode (ReadMode), withFile)

startCheckMemoryPoll ::
  EventLoopT Env (Device, ExCameraHandler) IO ()
startCheckMemoryPoll = do
  addMsg (Camera, ExCameraHandler (defMessage @Proto.CheckMemoryUsage))
  void $
    setInterval (Camera, ExCameraHandler (defMessage @Proto.CheckMemoryUsage)) $
      30 * 1000

startHomeConnection ::
  EventLoopT Env (Device, ExCameraHandler) IO ()
startHomeConnection = do
  addMsg (Camera, ExCameraHandler $ EstablishHomeConnection "127.0.0.1" 9000)

mainImpl :: IO ()
mainImpl = do
  -- runCommand $ \(opts :: CameraOptions) _args -> do
  bracket mkEnv cleanupEnv $ \env -> do
    runEventLoopT action env
 where
  action :: EventLoopT Env (Device, ExCameraHandler) IO ()
  action = do
    startHomeConnection
    startCheckMemoryPoll
    start $ uncurry cameraHandler

main :: IO ()
main =
  forever $
    catch mainImpl $
      \(SomeException e) -> do
        putStrLn $ "An exception occurred: " <> displayException e
