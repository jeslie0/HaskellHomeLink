{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Home.Env (
  Env,
  EnvT,
  mkEnv,
  audioStreamRef,
  router,
  addLocalHTTPServerConnection,
  addRemoteProxyConnection,
  cleanupEnv,
) where

import ConnectionManager (initTCPClientConnection, killConnections)
import Control.Concurrent (ThreadId, killThread)
import Control.Monad.Reader (ReaderT)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Home.AudioStream (StationId, StreamStatus (..))
import Islands (Island (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Msg (Msg)
import Network.Socket (HostName, ServiceName)
import Router (Router, connectionsManager, handleBytes, mkRouter)

data Env = Env
  { _router :: Router
  , _audioStreamRef :: IORef (Maybe ThreadId, StreamStatus, StationId)
  }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

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
  killConnections (env ^. (router . connectionsManager))

addLocalHTTPServerConnection ::
  forall msg. Msg msg => ((Island, msg) -> IO ()) -> Router -> IO ()
addLocalHTTPServerConnection actOnMsg rtr = do
  initTCPClientConnection
    LocalHTTP
    (rtr ^. connectionsManager)
    "127.0.0.1"
    "3000"
    (handleBytes actOnMsg rtr)
    (pure ())
    (pure ())

addRemoteProxyConnection ::
  forall msg.
  Msg msg =>
  ((Island, msg) -> IO ())
  -> HostName
  -> ServiceName
  -> Router
  -> IO ()
addRemoteProxyConnection actOnMsg host port rtr = do
  initTCPClientConnection
    RemoteProxy
    (rtr ^. connectionsManager)
    host
    port
    (handleBytes actOnMsg rtr)
    (pure ())
    (pure ())
