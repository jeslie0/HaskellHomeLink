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
) where

import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Devices (Device (..))
import Home.AudioStreamTypes (StationId, StreamStatus (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Logger (Logs, mkLogs)
import Network.WebSockets qualified as WS
import Router (Router, connectionsRegistry, mkRouter)
import RxTx.ConnectionRegistry (killConnections)
import State (State, mkState)
import System (DeviceData, mkDeviceData)
import System.Memory (MemoryInformation)

data Env = Env
  { _router :: Router
  , _streamStatusState :: State (StreamStatus, StationId)
  , _deviceMap :: MVar (Map.Map Device DeviceData)
  , _memoryMap :: MVar (Map.Map Device (V.Vector MemoryInformation))
  , _websocketsMap :: MVar (Map.Map Int32 WS.Connection)
  , _logs :: Logs
  }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: Device -> IO Env
mkEnv device = do
  _router <- mkRouter device
  _streamStatusState <- mkState (Off, 0)
  _httpServerMVar <- newEmptyMVar
  _deviceMap <- do
    Just deviceData <- mkDeviceData
    newMVar $ Map.insert device deviceData Map.empty
  _memoryMap <- newMVar Map.empty
  _websocketsMap <- newMVar Map.empty
  _logs <- mkLogs
  pure $
    Env
      { _router
      , _streamStatusState
      , _deviceMap
      , _memoryMap
      , _websocketsMap
      , _logs
      }

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
  killConnections (env ^. (router . connectionsRegistry))
