{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EventLoop (
  EventLoop,
  addMsgIO,
  EventLoopT,
  getLoop,
  MonadEventLoop (..),
  TimeoutId,
  IntervalId,
  runEventLoopT,
  setTimeoutIO,
  setIntervalIO,
  killIntervalIO,
  killTimeoutIO,
) where

import Control.Concurrent (
  Chan,
  MVar,
  ThreadId,
  forkFinally,
  isEmptyMVar,
  killThread,
  modifyMVar_,
  myThreadId,
  newChan,
  newEmptyMVar,
  newMVar,
  readChan,
  threadDelay,
  tryPutMVar,
  tryTakeMVar,
  writeChan,
 )
import Control.Monad (unless, void)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans
import Control.Monad.Trans.Control (
  MonadBaseControl (..),
  MonadTransControl (..),
 )
import Data.Set qualified as Set
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

-- | A single threaded event loop that can have messages added to it
-- from another thread.
data EventLoop msg = EventLoop
  { _shouldStopMVar :: MVar ()
  , _events :: Chan msg
  , _waitingThreads :: MVar (Set.Set ThreadId)
  }

$(makeLenses ''EventLoop)

addMsgIO :: msg -> EventLoop msg -> IO ()
addMsgIO msg loop =
  writeChan (loop ^. events) msg

setIntervalIO :: msg -> Int -> EventLoop msg -> IO IntervalId
setIntervalIO msg n loop = do
  threadId <- forkFinally action $ \case
    Left _ -> pure ()
    Right _ -> do
      thisThread <- myThreadId
      modifyMVar_ (loop ^. waitingThreads) $ pure . Set.delete thisThread
  modifyMVar_ (loop ^. waitingThreads) $ pure . Set.insert threadId
  pure $ IntervalId threadId
 where
  action = do
    threadDelay (n * 1000)
    writeChan (loop ^. events) msg
    action

setTimeoutIO :: msg -> Int -> EventLoop msg -> IO TimeoutId
setTimeoutIO msg n loop = do
  threadId <- forkFinally (threadDelay (n * 1000) >> writeChan (loop ^. events) msg) $ \case
    Left _ -> pure ()
    Right _ -> do
      thisThread <- myThreadId
      modifyMVar_ (loop ^. waitingThreads) $ pure . Set.delete thisThread
  modifyMVar_ (loop ^. waitingThreads) $ pure . Set.insert threadId
  pure $ TimeoutId threadId

killIntervalIO :: IntervalId -> IO ()
killIntervalIO (IntervalId threadId) = do
  killThread threadId

killTimeoutIO :: TimeoutId -> IO ()
killTimeoutIO (TimeoutId threadId) = do
  killThread threadId

mkEventLoop :: IO (EventLoop msg)
mkEventLoop = do
  shouldStop <- newEmptyMVar
  eventsChan <- newChan
  threads <- newMVar Set.empty
  pure $ EventLoop shouldStop eventsChan threads

newtype EventLoopT env msg m a
  = EventLoopT (ReaderT (EventLoop msg, env) m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (EventLoop msg, env)
    , MonadTrans
    , MonadFail
    , MonadTransControl
    )

newtype TimeoutId = TimeoutId ThreadId

newtype IntervalId = IntervalId ThreadId

-- | The MonadEventLoop class describes a computation returning value of
-- type a, running in the context of an event loop. Typically, a will
-- be ().
class MonadEventLoop env msg m | m -> env, m -> msg where
  getEnv :: m env

  addMsg :: msg -> m ()

  start :: (msg -> m ()) -> m ()

  stop :: m ()

  setInterval :: msg -> Int -> m IntervalId

  killInterval :: IntervalId -> m ()

  setTimeout :: msg -> Int -> m TimeoutId

  killTimeout :: TimeoutId -> m ()

instance MonadIO m => MonadEventLoop env msg (EventLoopT env msg m) where
  getEnv = EventLoopT $ do
    (_, env) <- ask
    pure env

  addMsg msg = EventLoopT $ do
    (loop, _) <- ask
    liftIO $ writeChan (loop ^. events) msg

  start dispatch = do
    loop <- getLoop
    void . liftIO $ tryPutMVar (loop ^. shouldStopMVar) ()
    go (loop ^. events) (loop ^. shouldStopMVar)
   where
    go chan stopperMVar = do
      shouldStop <- liftIO $ isEmptyMVar stopperMVar
      unless shouldStop $ do
        msg <- liftIO $ readChan chan
        dispatch msg
        go chan stopperMVar

  stop = EventLoopT $ do
    (loop, _) <- ask
    void . liftIO $ tryTakeMVar $ loop ^. shouldStopMVar

  setInterval msg n = do
    (loop, _) <- ask
    liftIO $ setIntervalIO msg n loop

  setTimeout msg n = EventLoopT $ do
    (loop, _) <- ask
    liftIO $ setTimeoutIO msg n loop

  killInterval intervalId = do
    liftIO $ killIntervalIO intervalId

  killTimeout timeoutId = do
    liftIO $ killTimeoutIO timeoutId

instance MonadBase b m => MonadBase b (EventLoopT env msg m) where
  liftBase = EventLoopT . liftBase

instance MonadBaseControl b m => MonadBaseControl b (EventLoopT env msg m) where
  type StM (EventLoopT env msg m) a = StM (ReaderT (EventLoop msg, env) m) a
  liftBaseWith f =
    EventLoopT $ liftBaseWith (\runInBase -> f (runInBase . coerce))
   where
    coerce (EventLoopT r) = r

  restoreM = EventLoopT . restoreM

getLoop :: Monad m => EventLoopT env msg m (EventLoop msg)
getLoop = EventLoopT $ do
  (loop, _) <- ask
  pure loop

runEventLoopT :: MonadIO m => EventLoopT env msg m a -> env -> m a
runEventLoopT (EventLoopT r) env = do
  loop <- liftIO mkEventLoop
  runReaderT r (loop, env)
