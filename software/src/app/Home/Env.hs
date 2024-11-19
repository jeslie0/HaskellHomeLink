{-# LANGUAGE TemplateHaskell #-}

module Home.Env (
    Env,
    EnvT,
    mkEnv,
    audioStreamMVar,
    connectionMVar,
    httpServerMVar
) where

import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Lens.Micro.TH (makeLenses)
import Threads (AsyncComputation)
import Connection (Connection)

data Env = Env
    { _audioStreamMVar :: MVar (Maybe AsyncComputation)
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
    pure $
        Env
            { _audioStreamMVar
            , _connectionMVar
            , _httpServerMVar
            }
