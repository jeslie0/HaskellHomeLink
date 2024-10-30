module Home.Env (Env (..), mkEnv) where

import Data.IORef (IORef, newIORef)

data Env = Env {_count :: IORef Int}

mkEnv :: IO Env
mkEnv = do
    ref <- newIORef 0
    return $ Env {_count = ref}
