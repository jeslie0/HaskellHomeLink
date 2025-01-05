module EventLoop (
  EventLoop,
  mkEventLoop,
  addMsg,
  setTimeout,
  run,
  killEventLoop,
) where

import Control.Concurrent (
  MVar,
  isEmptyMVar,
  newEmptyMVar,
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
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

-- | A single threaded event loop that can have messages added to it
-- from another thread.
data EventLoop m msg = EventLoop
  { _keepLoopAliveMVar :: MVar ()
  , _pqueueMVar :: MVar (PQueue.MinPQueue UTCTime msg)
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

  return
    EventLoop
      { _keepLoopAliveMVar
      , _pqueueMVar
      }

addMsg :: (MonadIO m, MonadIO m1) => EventLoop m1 msg -> msg -> m ()
addMsg loop msg = setTimeout loop msg 0

setTimeout :: (MonadIO m, MonadIO m1) => EventLoop m1 msg -> msg -> Int -> m ()
setTimeout (EventLoop {_keepLoopAliveMVar, _pqueueMVar}) msg timeoutMs = do
  time <- liftIO getCurrentTime
  -- Insert the messaage into the priority queue with
  -- current time.
  mPQueue <- liftIO $ tryTakeMVar _pqueueMVar
  case mPQueue of
    Nothing -> liftIO $ putMVar _pqueueMVar $ PQueue.singleton time msg
    Just queue ->
      liftIO $
        putMVar _pqueueMVar $
          (addUTCTime (fromIntegral timeoutMs) time, msg) PQueue.:< queue

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
