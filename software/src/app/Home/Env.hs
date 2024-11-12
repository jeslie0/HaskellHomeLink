module Home.Env (EnvT, Env (..), mkEnv) where

import Control.Concurrent (MVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Home.AudioStream
import ThreadPool (ThreadPool, mkThreadPool)

data Env = Env
    { _audioStream :: AudioStream
    , _threadPool :: ThreadPool
    , _connectionMVar :: MVar ()
    }

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    _threadPool <- mkThreadPool 100
    _audioStream <- mkAudioStream
    _connectionMVar <- newMVar ()

    return $
        Env
            { _audioStream
            , _threadPool
            , _connectionMVar
            }
