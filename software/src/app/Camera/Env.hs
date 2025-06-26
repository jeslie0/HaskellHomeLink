{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Env (
  Env,
  mkEnv,
  router,
  EnvT,
  cleanupEnv,
  videostreamRes,
  initChunk
) where

import Camera.VideoStream (VideoStreamResource, cleanupVideoStreamResource)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT)
import Data.ByteString qualified as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Devices (Device (Camera))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Router (Router, connectionsRegistry, mkRouter)
import RxTx.ConnectionRegistry (killConnections)

data Env = Env
  { _router :: Router
  , _videostreamRes :: IORef (Maybe VideoStreamResource)
  , _initChunk :: IORef (Maybe B.ByteString)
  }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
  _router <- mkRouter Camera
  _videostreamRes <- newIORef Nothing
  _initChunk <- newIORef Nothing
  pure $
    Env
      { _router
      , _videostreamRes
      , _initChunk
      }

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
  killConnections (env ^. (router . connectionsRegistry))
  videoRes <- readIORef $ env ^. videostreamRes
  forM_ videoRes cleanupVideoStreamResource
  writeIORef (env ^. videostreamRes) Nothing
