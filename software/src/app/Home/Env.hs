module Home.Env (Env (..), mkEnv) where

import Home.AudioStream
import Socket (SocketHandler)

data Env = Env
    { _audioStream :: AudioStream
    , _socketHandler :: SocketHandler
    }

mkEnv :: SocketHandler -> IO Env
mkEnv socketHandler = do
    audioStream <- mkAudioStream
    return $ Env {_audioStream = audioStream, _socketHandler = socketHandler}
