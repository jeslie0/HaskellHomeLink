{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Proxy.Env (Env, mkEnv, router, streamStatusState, EnvT, addLocalHTTPServerConnection) where

import ConnectionManager (Island (..), initTCPServerConnection)
import Control.Concurrent (newEmptyMVar)
import Control.Monad.Reader (ReaderT)
import Home.AudioStream (StreamStatus (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Msg (Msg)
import Router (Router, connectionsManager, handleBytes, mkRouter)
import State (State, mkState)

data Env = Env
    { _router :: Router
    , _streamStatusState :: State StreamStatus
    }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: Island -> IO Env
mkEnv island = do
    _router <- mkRouter island
    _streamStatusState <- mkState Inactive
    _httpServerMVar <- newEmptyMVar
    pure $
        Env
            { _router
            , _streamStatusState
            }

addLocalHTTPServerConnection ::
    forall msg. (Msg msg) => ((Island, msg) -> IO ()) -> Router -> IO ()
addLocalHTTPServerConnection actOnMsg rtr = do
    initTCPServerConnection Home (rtr ^. connectionsManager) "3000" $ handleBytes actOnMsg rtr
