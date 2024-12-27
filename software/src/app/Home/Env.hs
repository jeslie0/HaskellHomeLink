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
) where

import ConnectionManager (Island (..), initTCPClientConnection)
import Control.Concurrent (ThreadId)
import Control.Monad.Reader (ReaderT)
import Data.IORef (IORef, newIORef)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Msg (Msg)
import Network.Socket (HostName, ServiceName)
import Router (Router, connectionsManager, handleBytes, mkRouter)

data Env = Env
    { _router :: Router
    , _audioStreamRef :: IORef (Maybe ThreadId)
    }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    _audioStreamRef <- newIORef Nothing
    _router <- mkRouter Home
    pure $
        Env
            { _audioStreamRef
            , _router
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
