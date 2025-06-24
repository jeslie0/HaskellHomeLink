{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Env (
  Env,
  mkEnv,
  router,
  EnvT,
  cleanupEnv,
  videostreamRes
) where

import Devices (Device (Camera))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Router (Router, mkRouter, connectionsRegistry)
import Control.Monad.Reader (ReaderT)
import RxTx.ConnectionRegistry (killConnections)
import Camera.VideoStream (VideoStreamResource, cleanupVideoStreamResource)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (forM_)
  
data Env = Env
  { _router :: Router
  , _videostreamRes :: IORef (Maybe VideoStreamResource)
  }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
  _router <- mkRouter Camera
  _videostreamRes <- newIORef Nothing
  pure $
    Env
      { _router
      , _videostreamRes
      }

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
  killConnections (env ^. (router . connectionsRegistry))
  videoRes <- readIORef $ env ^. videostreamRes
  forM_ videoRes cleanupVideoStreamResource
  writeIORef (env ^. videostreamRes) Nothing
