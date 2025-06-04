{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Home.Env (
  Env,
  mkEnv,
  audioStreamRef,
  router,
  -- addRemoteProxyConnection,
  cleanupEnv,
) where

import Control.Concurrent (ThreadId, killThread)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Home.AudioStream (StationId, StreamStatus (..))
import Islands (Island (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Router (Router, connectionsRegistry, mkRouter)
import RxTx.ConnectionRegistry (killConnections)

data Env = Env
  { _router :: Router
  , _audioStreamRef :: IORef (Maybe ThreadId, StreamStatus, StationId)
  }

$(makeLenses ''Env)

mkEnv :: IO Env
mkEnv = do
  _audioStreamRef <- newIORef (Nothing, Off, 0)
  _router <- mkRouter Home
  pure $
    Env
      { _audioStreamRef
      , _router
      }

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
  (mThread, _, _) <- readIORef (env ^. audioStreamRef)
  for_ mThread killThread
  writeIORef (env ^. audioStreamRef) (Nothing, Off, 0)
  killConnections (env ^. (router . connectionsRegistry))
