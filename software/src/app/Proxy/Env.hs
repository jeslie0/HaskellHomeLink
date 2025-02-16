{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Proxy.Env (
  Env,
  mkEnv,
  router,
  streamStatusState,
  EnvT,
  addServerConnection,
  systemMap,
  memoryMap,
  logs,
  cleanupEnv,
  addTLSServerConnection,
) where

import ConnectionManager (
  initTCPServerConnection,
  initTLSServerConnection,
  killConnections,
 )
import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Home.AudioStreamTypes (StationId, StreamStatus (..))
import Islands (Island (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Logger (Logs, mkLogs)
import Msg (Msg)
import Network.Socket (Socket)
import Network.TLS (ServerParams)
import Router (Router, connectionsManager, handleBytes, mkRouter)
import State (State, mkState)
import System (SystemData, mkSystemData)
import System.Memory (MemoryInformation)

data Env = Env
  { _router :: Router
  , _streamStatusState :: State (StreamStatus, StationId)
  , _systemMap :: MVar (Map.Map Island SystemData)
  , _memoryMap :: MVar (Map.Map Island (V.Vector MemoryInformation))
  , _logs :: Logs
  }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: Island -> IO Env
mkEnv island = do
  _router <- mkRouter island
  _streamStatusState <- mkState (Off, 0)
  _httpServerMVar <- newEmptyMVar
  _systemMap <- do
    systemData <- mkSystemData $ if island == LocalHTTP then Home else island
    newMVar $ Map.insert island systemData Map.empty
  _memoryMap <- newMVar Map.empty
  _logs <- mkLogs
  pure $
    Env
      { _router
      , _streamStatusState
      , _systemMap
      , _memoryMap
      , _logs
      }

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
  killConnections (env ^. (router . connectionsManager))

addServerConnection ::
  forall msg.
  Msg msg =>
  ((Island, msg) -> IO ())
  -> Router
  -> IO ()
  -> IO ()
  -> IO Socket
addServerConnection actOnMsg rtr = do
  initTCPServerConnection
    Home
    (rtr ^. connectionsManager)
    "3001"
    (handleBytes actOnMsg rtr)

addTLSServerConnection ::
  forall msg.
  Msg msg =>
  ServerParams
  -> ((Island, msg) -> IO ())
  -> Router
  -> IO ()
  -> IO ()
  -> IO Socket
addTLSServerConnection params actOnMsg rtr = do
  initTLSServerConnection
    params
    Home
    (rtr ^. connectionsManager)
    "3001"
    (handleBytes actOnMsg rtr)
