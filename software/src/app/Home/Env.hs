module Home.Env (EnvT, Env (..), mkEnv) where

import Connection
import Control.Concurrent (MVar, ThreadId, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Home.AudioStream

data Env = Env
    { _audioStream :: AudioStream
    , _conn :: MVar Connection
    , _connThread :: (MVar ThreadId, MVar ())
    }

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    audioStream <- mkAudioStream
    connMVar <- newEmptyMVar
    connThread <-
        newEmptyMVar >>= \threadMVar -> newMVar () >>= \blockMVar -> pure (threadMVar, blockMVar)
    return $
        Env
            { _audioStream = audioStream
            , _conn = connMVar
            , _connThread = connThread
            }
