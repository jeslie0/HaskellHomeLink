{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Home.Env (
  Env,
  mkEnv,
  audioStreamRef,
  router,
  cleanupEnv,
) where

import Control.Concurrent (ThreadId, killThread)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Devices (Device (..))
import HAsio.Error.ErrorStack (ErrorStack)
import Home.AudioStream (StationId, StreamStatus (..))
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Router (Router, connectionsRegistry, mkRouter)
import RxTx.ConnectionRegistry (killConnections)

data Env = Env
  { _router :: Router
  , _audioStreamRef :: IORef (Maybe ThreadId, StreamStatus, StationId)
  }

$(makeLenses ''Env)

mkEnv :: ExceptT ErrorStack IO Env
mkEnv = do
  _audioStreamRef <- liftIO $ newIORef (Nothing, Off, 0)
  _router <- liftIO $ mkRouter Home
  pure $ Env _router _audioStreamRef

cleanupEnv :: Env -> ExceptT ErrorStack IO ()
cleanupEnv env = liftIO $ do
  (mThread, _, _) <- readIORef (env ^. audioStreamRef)
  for_ mThread killThread
  writeIORef (env ^. audioStreamRef) (Nothing, Off, 0)
  killConnections (env ^. (router . connectionsRegistry))
