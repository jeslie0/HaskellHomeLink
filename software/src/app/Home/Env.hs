{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Home.Env (
  Env,
  mkEnv,
  audioStreamRef,
  router,
  cleanupEnv,
  cameraServerSocket,
) where

import Control.Concurrent (ThreadId, killThread)
import Control.Exception (bracketOnError)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Devices (Device (..))
import Home.AudioStream (StationId, StreamStatus (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Ports (homeCameraRecvPort)
import Router (Router, connectionsRegistry, mkRouter)
import RxTx.Connection.Socket (aquireBoundListeningServerSocket)
import RxTx.ConnectionRegistry (killConnections)

data Env = Env
  { _router :: Router
  , _audioStreamRef :: IORef (Maybe ThreadId, StreamStatus, StationId)
  , _cameraServerSocket :: Socket
  }

$(makeLenses ''Env)

mkEnv :: IO Env
mkEnv = do
  _audioStreamRef <- newIORef (Nothing, Off, 0)
  _cameraServerSocket <- newIORef Nothing
  _router <- mkRouter Home
  bracketOnError
    (aquireBoundListeningServerSocket $ show homeCameraRecvPort)
    Socket.close $ \_cameraServerSocket ->
    pure $
      Env
        { _audioStreamRef
        , _router
        , _cameraServerSocket
        }

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
  (mThread, _, _) <- readIORef (env ^. audioStreamRef)
  for_ mThread killThread
  writeIORef (env ^. audioStreamRef) (Nothing, Off, 0)
  killConnections (env ^. (router . connectionsRegistry))
  Socket.close (env ^. cameraServerSocket)
