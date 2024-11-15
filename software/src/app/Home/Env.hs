{-# LANGUAGE TemplateHaskell #-}

module Home.Env (
    Env,
    EnvT,
    mkEnv,
    audioStream,
    connectionMVar,
) where

import Control.Concurrent (MVar, newEmptyMVar)
import Control.Monad.Reader (ReaderT)
import Home.AudioStream
import Lens.Micro.TH (makeLenses)
import Threads (AsyncComputation)

data Env = Env
    { _audioStream :: AudioStream
    , _connectionMVar :: MVar AsyncComputation
    }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    _audioStream <- mkAudioStream
    _connectionMVar <- newEmptyMVar

    return $
        Env
            { _audioStream
            , _connectionMVar
            }
