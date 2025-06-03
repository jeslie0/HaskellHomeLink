module RxTx.Connection (
  SomeConnection (..),
  mkConnection,
  Connection (..),
  runSomeConnection,
  AsyncSomeConnection (..),
) where

import Control.Concurrent (ThreadId)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef qualified as IORef
import RxTx (Rx (..), RxTx, Tx (..))

data Connection msg rxErr txErr = Connection
  { send :: msg -> IO (Either txErr ())
  , addSub :: (msg -> IO ()) -> IO ()
  , recvAndDispatch :: IO (Maybe rxErr)
  , cleanup :: IO ()
  }

mkConnection ::
  forall msg chan rxErr txErr.
  (RxTx msg chan rxErr txErr) =>
  chan
  -> (chan -> IO ())
  -> IO (Connection msg rxErr txErr)
mkConnection chan cleanup = do
  subsRef <- liftIO $ IORef.newIORef []
  pure $
    Connection
      { send = sendMsg chan
      , addSub = \sub -> liftIO $ IORef.atomicModifyIORef' subsRef $ \subs -> (sub : subs, ())
      , recvAndDispatch = do
          eMsg <- recvMsg chan
          case eMsg of
            Left err -> pure $ Just err
            Right msg -> do
              subs <- liftIO $ IORef.readIORef subsRef
              forM_ subs (\sub -> sub msg)
              pure Nothing
      , cleanup = cleanup chan
      }

data SomeConnection msg
  = forall rxErr txErr. SomeConnection (Connection msg rxErr txErr)

runSomeConnection :: SomeConnection msg -> IO ()
runSomeConnection conn@(SomeConnection (Connection {recvAndDispatch})) = do
  result <- recvAndDispatch
  case result of
    Nothing -> runSomeConnection conn
    Just _ -> pure ()

data AsyncSomeConnection msg = AsyncSomeConnection ThreadId (SomeConnection msg)
