module Home.Env (Env (..), mkEnv) where

import Data.IORef (IORef, newIORef)
import Home.AudioStream

data Env = Env {_audioStream :: AudioStream}

mkEnv :: IO Env
mkEnv = do
    ref <- newIORef 0
    audioStream <- mkAudioStream
    return $ Env {_audioStream = audioStream}
