{-# LANGUAGE MonoLocalBinds #-}

module Home.Env (
  Env,
  mkEnv,
  router,
  cleanupEnv,
  withEnv,
) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef)
import Devices (Device (..))
import HAsio.Async.TLS qualified as TLS
import HAsio.Control (bracket)
import HAsio.Error.ErrorStack (ErrorStack)
import HAsio.Fd.Socket (Socket)
import HAsio.Fd.Syscalls (close')
import Router (Router, mkRouter)

data Env = Env
  { router :: Router
  , cameraServerSocket :: Socket
  , cameraConnectionRef :: IORef (Maybe Socket)
  , proxyConnectionRef :: IORef (Maybe TLS.TLSConnection)
  -- , _audioStreamRef :: IORef (Maybe ThreadId, StreamStatus, StationId)
  }

mkEnv :: ExceptT ErrorStack IO Env
mkEnv = do
  router <- liftIO $ mkRouter Home
  cameraConnectionRef <- liftIO $ newIORef Nothing
  proxyConnectionRef <- liftIO $ newIORef Nothing

  pure $
    Env
      { router
      , cameraServerSocket = undefined
      , cameraConnectionRef
      , proxyConnectionRef
      }

cleanupEnv :: Env -> ExceptT ErrorStack IO ()
cleanupEnv env = do
  liftIO (readIORef (cameraConnectionRef env)) >>= traverse_ close'
  liftIO (readIORef (proxyConnectionRef env)) >>= traverse_ TLS.close
  close' (cameraServerSocket env)

withEnv ::
  (Env -> ExceptT ErrorStack IO ()) -> ExceptT ErrorStack IO ()
withEnv =
  bracket mkEnv cleanupEnv
