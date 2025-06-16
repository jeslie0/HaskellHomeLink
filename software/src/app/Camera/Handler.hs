{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Handler (
) where

import Control.Concurrent (modifyMVar_)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Envelope
import EventLoop (EventLoopT, getEnv)
import Lens.Micro ((&), (.~), (^.))
import Logger (LogLevel (..), addLog, reportLog)
import ProtoHelper (FromMessage (..), toMessage)
import Router (trySendMessage)
import State (fulfilPromise)
import System.Memory (MemoryInformation, getMemoryInformation)
import TH (makeInstance)

-- class CameraHandler msg where
--   cameraHandler ::
--     Island -> msg -> EventLoopT Env (Island, ExCameraHandler) IO ()

-- data ExCameraHandler = forall a. CameraHandler a => ExCameraHandler a

-- instance CameraHandler ExCameraHandler where
--   cameraHandler island (ExCameraHandler msg) = cameraHandler island msg

-- * Message instances

-- -- Make CameraHandler Proto.ProxyRecieveEnvelope instance.
-- $( makeInstance
--     ''CameraHandler
--     ''Proto.CameraEnvelope
--     'Proto.maybe'payload
--     ''Proto.CameraEnvelope'Payload
--  )

-- -- | Received acknowledgement from Home for ModifyRadioRequest
-- instance CameraHandler Proto.TakePicture where
--   cameraHandler _ resp = do
--     env <- getEnv
--     liftIO $ do
--       mBytes <- takePicture
--       case mBytes of
--         Nothing -> reportLog (env ^. router) Error "Failed to take picture"
--         Just bytes ->
--           void $ trySendMessage (env ^. router) RemoteProxy (makeMsg bytes)
--    where
--     makeMsg bytes = defMessage @Proto.PictureData & Proto.imageData .~ bytes
