module EventLoop (EventLoop (..), addMsg, killEventLoop, mkEventLoop) where

import Control.Concurrent (
    killThread,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryTakeMVar,
 )
import Control.Concurrent.Lifted (fork)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.PQueue.Prio.Min qualified as PQueue
import Data.Time.Clock.System (SystemTime, getSystemTime)

{- | A single threaded event loop that can have messages added to it
from another thread.
-}
data EventLoop msg = EventLoop
    { _addMsg :: msg -> IO ()
    , _killEventLoop :: IO ()
    }

-- | Add a new message to be handled.
addMsg :: (MonadIO m) => EventLoop msg -> msg -> m ()
addMsg loop = liftIO . _addMsg loop

-- | Stop the event loop.
killEventLoop :: (MonadIO m) => EventLoop msg -> m ()
killEventLoop = liftIO . _killEventLoop

-- | Create an event loop.
mkEventLoop ::
    forall msg m.
    (MonadIO m, MonadBaseControl IO m) =>
    (msg -> m ())
    -> m (EventLoop msg)
mkEventLoop handle = do
    -- Block checking the state of the queue. To be used when the queue
    -- is empty.
    pauseLoopMVar <- liftIO $ newEmptyMVar @()

    -- MVar around the piority queue
    pqueueMVar <-
        liftIO . newMVar $ PQueue.empty @SystemTime @msg

    threadId <- fork $ actOnQueue pauseLoopMVar pqueueMVar

    let addMsg' msg = do
            time <- getSystemTime
            -- Insert the messaage into the priority queue with
            -- current time.
            modifyMVar_ pqueueMVar $ \queue -> pure $ (time, msg) PQueue.:< queue
            -- Unblock the loop if it was waiting
            void $ tryPutMVar pauseLoopMVar ()

    let killEventLoop' = do
            liftIO . void $ tryPutMVar pauseLoopMVar ()
            killThread threadId

    return $
        EventLoop
            { _addMsg = addMsg'
            , _killEventLoop = killEventLoop'
            }
  where
    actOnQueue pauseLoopMVar mvar = do
        -- This blocks until pauseLoopMVar is filled, which happens
        -- when we add an event.
        _ <- liftIO $ readMVar pauseLoopMVar
        queue <- liftIO $ takeMVar mvar
        case queue of
            PQueue.Empty -> do
                liftIO . void $ tryTakeMVar pauseLoopMVar
                liftIO $ putMVar mvar PQueue.Empty
            (PQueue.:<) (msgTime, msg) remQueue -> do
                curTime <- liftIO getSystemTime
                if msgTime <= curTime
                    then do
                        -- Make sure to put the remaining queue back to
                        -- not block other messages being added by other threads.
                        liftIO $ putMVar mvar remQueue
                        handle msg
                    else liftIO $ putMVar mvar queue
        actOnQueue pauseLoopMVar mvar
