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
    systemMap,
) where

import ConnectionManager (Island (..), initTCPClientConnection)
import Control.Concurrent (ThreadId)
import Control.Monad.Reader (ReaderT)
import Data.IORef (IORef, newIORef)
import Data.Map.Strict qualified as Map
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Msg (Msg)
import Network.Socket (HostName, ServiceName)
import Router (Router, connectionsManager, handleBytes, mkRouter)
import System (SystemData, mkSystemData)

data Env = Env
    { _router :: Router
    , _audioStreamRef :: IORef (Maybe ThreadId)
    , _systemMap :: IORef (Map.Map Island SystemData)
    }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    _audioStreamRef <- newIORef Nothing
    _router <- mkRouter Home
    _systemMap <- do
        mHomeSystemData <- mkSystemData
        mp <- case mHomeSystemData of
            Nothing -> pure Map.empty
            Just homeSystemData -> pure $ Map.insert Home homeSystemData Map.empty
        newIORef mp
    pure $
        Env
            { _audioStreamRef
            , _router
            , _systemMap
            }

addLocalHTTPServerConnection ::
    forall msg. (Msg msg) => ((Island, msg) -> IO ()) -> Router -> IO ()
addLocalHTTPServerConnection actOnMsg rtr = do
    initTCPClientConnection LocalHTTP (rtr ^. connectionsManager) "127.0.0.1" "3000" $
        handleBytes actOnMsg rtr

addRemoteProxyConnection ::
    forall msg.
    (Msg msg) =>
    ((Island, msg) -> IO ())
    -> HostName
    -> ServiceName
    -> Router
    -> IO ()
addRemoteProxyConnection actOnMsg host port rtr = do
    initTCPClientConnection RemoteProxy (rtr ^. connectionsManager) host port $
        handleBytes actOnMsg rtr
