{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Proxy.Env (
  Env,
  mkEnv,
  router,
  streamStatusState,
  EnvT,
  deviceMap,
  memoryMap,
  logs,
  cleanupEnv,
  websocketsMap,
  serverSocket
) where

import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Control.Exception (bracketOnError)
import Control.Monad.Reader (ReaderT)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Devices (Device (..))
import Home.AudioStreamTypes (StationId, StreamStatus (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Logger (Logs, mkLogs)
import Network.Socket (ServiceName, Socket, close)
import Network.WebSockets qualified as WS
import Router (Router, connectionsRegistry, mkRouter)
import RxTx.ConnectionRegistry (killConnections)
import State (State, mkState)
import System (DeviceData, mkDeviceData)
import System.Memory (MemoryInformation)
import RxTx.Connection.Socket (aquireBoundListeningServerSocket)

data Env = Env
  { _router :: Router
  , _streamStatusState :: State (StreamStatus, StationId)
  , _deviceMap :: MVar (Map.Map Device DeviceData)
  , _memoryMap :: MVar (Map.Map Device (V.Vector MemoryInformation))
  , _websocketsMap :: MVar (Map.Map Int32 WS.Connection)
  , _serverSocket :: Socket
  , _logs :: Logs
  }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: ServiceName -> IO Env
mkEnv port = do
  _router <- mkRouter Proxy
  _streamStatusState <- mkState (Off, 0)
  _httpServerMVar <- newEmptyMVar
  _deviceMap <- do
    Just deviceData <- mkDeviceData
    newMVar $ Map.insert Proxy deviceData Map.empty
  _memoryMap <- newMVar Map.empty
  _websocketsMap <- newMVar Map.empty
  _logs <- mkLogs
  bracketOnError (aquireBoundListeningServerSocket port) close $ \serverSock ->
    pure $
      Env
        { _router
        , _streamStatusState
        , _deviceMap
        , _memoryMap
        , _websocketsMap
        , _logs
        , _serverSocket = serverSock
        }

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
  killConnections (env ^. (router . connectionsRegistry))
  close (env ^. serverSocket)
