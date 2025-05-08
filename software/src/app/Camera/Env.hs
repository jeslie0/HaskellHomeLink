{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.Env (
  Env,
  mkEnv,
  router,
  EnvT,
) where

import Islands (Island)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Router (Router, mkRouter, connectionsManager)
import Control.Monad.Reader (ReaderT)
import ConnectionManager (killConnections)
  
data Env = Env
  { _router :: Router
  }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: Island -> IO Env
mkEnv island = do
  _router <- mkRouter island
  pure $
    Env
      { _router
      }

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
  killConnections (env ^. (router . connectionsManager))
