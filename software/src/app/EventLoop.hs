module EventLoop (
  EventLoop,
  mkEventLoop,
  addMsg,
  setTimeout,
  run,
  killEventLoop,
  TimeoutId,
  killTimeout,
  setInterval,
  killInterval,
  IntervalId,
) where

import Control.Concurrent (
  MVar,
  ThreadId,
  forkFinally,
  isEmptyMVar,
  killThread,
  modifyMVar,
  myThreadId,
  newEmptyMVar,
  newMVar,
  putMVar,
  takeMVar,
  threadDelay,
  tryTakeMVar,
 )
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.PQueue.Prio.Min qualified as PQueue
import Data.Time.Clock (UTCTime, getCurrentTime)

-- | A single threaded event loop that can have messages added to it
-- from another thread.
data EventLoop m msg = EventLoop
  { _keepLoopAliveMVar :: MVar ()
  , _pqueueMVar :: MVar (PQueue.MinPQueue UTCTime msg)
  , _waitingThreads :: MVar [ThreadId]
  }

-- | Create an event loop.
mkEventLoop ::
  forall msg m.
  (MonadIO m, MonadBaseControl IO m) =>
  m (EventLoop m msg)
mkEventLoop = do
  -- Kill the loop when this mvar is empty.
  _keepLoopAliveMVar <- liftIO $ newEmptyMVar @()

  -- MVar around the priority queue. We use the MVar to determine if
  -- the queue is empty or not. An empty MVar means an empty queue.
  _pqueueMVar <- liftIO newEmptyMVar

  _waitingThreads <- liftIO $ newMVar []

  return
    EventLoop
      { _keepLoopAliveMVar
      , _pqueueMVar
      , _waitingThreads
      }

addMsg :: (MonadIO m, MonadIO m1) => EventLoop m1 msg -> msg -> m ()
addMsg (EventLoop {_keepLoopAliveMVar, _pqueueMVar}) msg = do
  time <- liftIO getCurrentTime
  mPQueue <- liftIO $ tryTakeMVar _pqueueMVar
  case mPQueue of
    Nothing -> liftIO $ putMVar _pqueueMVar $ PQueue.singleton time msg
    Just queue ->
      liftIO . putMVar _pqueueMVar $ (time, msg) PQueue.:< queue

newtype TimeoutId = TimeoutId ThreadId

setTimeout ::
  (MonadIO m, MonadIO m1) => EventLoop m1 msg -> msg -> Int -> m TimeoutId
setTimeout loop@(EventLoop {_keepLoopAliveMVar, _pqueueMVar, _waitingThreads}) msg timeoutMs = do
  newThread <-
    liftIO
      . forkFinally (threadDelay (timeoutMs * 1000) >> addMsg loop msg)
      $ ( \_ -> do
            thisThread <- myThreadId
            modifyMVar _waitingThreads $ \threads -> pure (filter (/= thisThread) threads, ())
        )
  liftIO . modifyMVar _waitingThreads $ \threads -> pure (newThread : threads, TimeoutId newThread)

killTimeout :: MonadIO m => TimeoutId -> m ()
killTimeout (TimeoutId threadId) = liftIO $ killThread threadId

newtype IntervalId = IntervalId ThreadId

setInterval ::
  (MonadIO m, MonadIO m1) => EventLoop m1 msg -> msg -> Int -> m IntervalId
setInterval loop@(EventLoop {_keepLoopAliveMVar, _pqueueMVar, _waitingThreads}) msg timeoutMs = do
  let action = do
        threadDelay (timeoutMs * 1000)
        addMsg loop msg
        action
  newThread <-
    liftIO
      . forkFinally
        action
      $ ( \_ -> do
            thisThread <- myThreadId
            modifyMVar _waitingThreads $ \threads -> pure (filter (/= thisThread) threads, ())
        )
  liftIO . modifyMVar _waitingThreads $ \threads -> pure (newThread : threads, IntervalId newThread)

killInterval :: MonadIO m => IntervalId -> m ()
killInterval (IntervalId threadId) = liftIO $ killThread threadId

killEventLoop :: MonadIO m => EventLoop m msg -> m ()
killEventLoop (EventLoop {_keepLoopAliveMVar, _pqueueMVar}) = do
  liftIO $ putMVar _keepLoopAliveMVar ()

run ::
  MonadIO m =>
  EventLoop m msg
  -> (EventLoop m msg -> msg -> m ())
  -> m ()
run loop@(EventLoop {_keepLoopAliveMVar, _pqueueMVar}) handle =
  actOnQueue
 where
  actOnQueue = do
    keepLoopAlive <- liftIO $ isEmptyMVar _keepLoopAliveMVar
    when keepLoopAlive $ do
      -- This blocks until pauseLoopMVar is filled, which happens
      -- when we add an event.
      queue <- liftIO $ takeMVar _pqueueMVar
      case queue of
        PQueue.Empty -> do
          liftIO $ putStrLn "This should never happen"
          liftIO . throwIO $ userError "Empty event loop queue detected"
        (PQueue.:<) (msgTime, msg) remQueue -> do
          curTime <- liftIO getCurrentTime
          if msgTime <= curTime
            then do
              -- If the remaning queue is empty, don't put it
              -- back in the MVar.
              unless (null remQueue) (liftIO $ putMVar _pqueueMVar remQueue)
              handle loop msg
            else do
              -- If the time isn't up yet, try waiting a
              -- short perid of time before checking again.
              liftIO $ putMVar _pqueueMVar queue
              liftIO $ threadDelay 1000
      actOnQueue
