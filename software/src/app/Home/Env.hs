module Home.Env (EnvT, Env (..), mkEnv) where

import Connection
import Control.Concurrent (MVar, ThreadId, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Home.AudioStream
import ThreadPool (ThreadPool, mkThreadPool)

data Env = Env
    { _audioStream :: AudioStream
    , _threadPool :: ThreadPool
    }

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    _threadPool <- mkThreadPool 8
    _audioStream <- mkAudioStream

    return $
        Env
            { _audioStream
            , _threadPool
            }
