{-# LANGUAGE TemplateHaskell #-}

module Home.Env (
    Env,
    EnvT,
    mkEnv,
    audioStreamMVar,
    connectionMVar,
    httpServerMVar
) where

import Control.Concurrent (MVar, newEmptyMVar)
import Control.Monad.Reader (ReaderT)
import Lens.Micro.TH (makeLenses)
import Threads (AsyncComputation)

data Env = Env
    { _audioStreamMVar :: MVar AsyncComputation
    , _connectionMVar :: MVar AsyncComputation
    , _httpServerMVar :: MVar AsyncComputation
    }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    _audioStreamMVar <- newEmptyMVar
    _connectionMVar <- newEmptyMVar
    _httpServerMVar <- newEmptyMVar
    pure $
        Env
            { _audioStreamMVar
            , _connectionMVar
            , _httpServerMVar
            }
