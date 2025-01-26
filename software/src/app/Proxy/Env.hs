{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Proxy.Env (
  Env,
  mkEnv,
  router,
  streamStatusState,
  EnvT,
  addLocalHTTPServerConnection,
  systemMap,
  memoryMap,
  logs
) where

import ConnectionManager (initTCPServerConnection)
import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Home.AudioStream (StationId, StreamStatus (..))
import Islands (Island (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Msg (Msg)
import Router (Router, connectionsManager, handleBytes, mkRouter)
import State (State, mkState)
import System (SystemData, mkSystemData)
import System.Memory (MemoryInformation)
import Logger (Logs, mkLogs)

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
    mHomeSystemData <- mkSystemData
    mp <- case mHomeSystemData of
      Nothing -> putStrLn "Could not extract cpuinfo" >> pure Map.empty
      Just homeSystemData -> pure $ Map.insert Home homeSystemData Map.empty
    newMVar mp
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

addLocalHTTPServerConnection ::
  forall msg. Msg msg => ((Island, msg) -> IO ()) -> Router -> IO ()
addLocalHTTPServerConnection actOnMsg rtr = do
  initTCPServerConnection Home (rtr ^. connectionsManager) "3000" $
    handleBytes actOnMsg rtr
