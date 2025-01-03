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
) where

import ConnectionManager (Island (..), initTCPServerConnection)
import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Data.Map.Strict qualified as Map
import Home.AudioStream (StreamStatus (..), StreamId)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Msg (Msg)
import Router (Router, connectionsManager, handleBytes, mkRouter)
import State (State, mkState)
import System (SystemData, mkSystemData)

data Env = Env
    { _router :: Router
    , _streamStatusState :: State (StreamStatus, StreamId)
    , _systemMap :: MVar (Map.Map Island SystemData)
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
    pure $
        Env
            { _router
            , _streamStatusState
            , _systemMap
            }

addLocalHTTPServerConnection ::
    forall msg. (Msg msg) => ((Island, msg) -> IO ()) -> Router -> IO ()
addLocalHTTPServerConnection actOnMsg rtr = do
    initTCPServerConnection Home (rtr ^. connectionsManager) "3000" $
        handleBytes actOnMsg rtr
