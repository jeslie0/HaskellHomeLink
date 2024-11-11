module EventLoop (EventLoop (..), mkEventLoop) where

import Control.Concurrent (
    isEmptyMVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
    tryTakeMVar,
 )
import Control.Exception (throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.PQueue.Prio.Min qualified as PQueue
import Data.Time.Clock.System (getSystemTime)

{- | A single threaded event loop that can have messages added to it
from another thread.
-}
data EventLoop m msg = EventLoop
    { _addMsg :: msg -> IO ()
    , _killEventLoop :: IO ()
    , _run :: m ()
    }

-- | Create an event loop.
mkEventLoop ::
    forall msg m.
    (MonadIO m, MonadBaseControl IO m) =>
    (EventLoop m msg -> msg -> m ())
    -> m (EventLoop m msg)
mkEventLoop handle = do
    -- Kill the loop when this mvar is empty.
    keepLoopAliveMVar <- liftIO $ newEmptyMVar @()

    -- MVar around the priority queue. We use the MVar to determine if
    -- the queue is empty or not. An empty MVar means an empty queue.
    pqueueMVar <-
        liftIO newEmptyMVar

    let _addMsg' msg = do
            time <- getSystemTime
            -- Insert the messaage into the priority queue with
            -- current time.
            mPQueue <- tryTakeMVar pqueueMVar
            case mPQueue of
                Nothing -> putMVar pqueueMVar $ PQueue.singleton time msg
                Just queue -> putMVar pqueueMVar $ (time, msg) PQueue.:< queue

        _killEventLoop' = do
            liftIO . void $ putMVar keepLoopAliveMVar ()

        _run' = actOnQueue keepLoopAliveMVar pqueueMVar eventLoop

        eventLoop =
            EventLoop
                { _addMsg = _addMsg'
                , _killEventLoop = _killEventLoop'
                , _run = _run'
                }

    return eventLoop
  where
    actOnQueue keepLoopAliveMVar pqueueMVar eventLoop = do
        keepLoopAlive <- liftIO $ isEmptyMVar keepLoopAliveMVar
        when keepLoopAlive $ do
            -- This blocks until pauseLoopMVar is filled, which happens
            -- when we add an event.
            queue <- liftIO $ takeMVar pqueueMVar
            case queue of
                PQueue.Empty -> do
                    liftIO $ putStrLn "This should never happen"
                    liftIO . throwIO $ userError "Empty event lopp queue detected"
                (PQueue.:<) (msgTime, msg) remQueue -> do
                    curTime <- liftIO getSystemTime
                    if msgTime <= curTime
                        then do
                            -- If the remaning queue is empty, don't put it
                            -- back in the MVar.
                            unless (null remQueue) (liftIO $ putMVar pqueueMVar remQueue)
                            handle eventLoop msg
                        else liftIO $ putMVar pqueueMVar queue
            actOnQueue keepLoopAliveMVar pqueueMVar eventLoop
