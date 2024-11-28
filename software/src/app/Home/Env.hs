{-# LANGUAGE TemplateHaskell #-}

module Home.Env (
    Env,
    EnvT,
    mkEnv,
    audioStreamMVar,
    connectionMVar,
    httpServerMVar,
    router,
) where

import Connection (Connection)
import ConnectionManager (Island (..))
import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Lens.Micro.TH (makeLenses)
import Router (Router, mkRouter)
import Threads (AsyncComputation)

data Env = Env
    { _router :: Router
    , _audioStreamMVar :: MVar (Maybe AsyncComputation)
    , _connectionMVar :: MVar Connection
    , _httpServerMVar :: MVar AsyncComputation
    }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    _audioStreamMVar <- newMVar Nothing
    _connectionMVar <- newEmptyMVar
    _httpServerMVar <- newEmptyMVar
    _router <- mkRouter Home
    pure $
        Env
            { _audioStreamMVar
            , _connectionMVar
            , _httpServerMVar
            , _router
            }
