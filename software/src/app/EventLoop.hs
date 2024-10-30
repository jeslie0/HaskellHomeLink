{-# LANGUAGE MonoLocalBinds #-}

module EventLoop (EventLoop (..), addMsg, killEventLoop, mkEventLoop) where

import Control.Concurrent (killThread, newChan, readChan, writeChan)
import Control.Concurrent.Lifted (fork)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)

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
    chan <- liftIO $ newChan @msg
    threadId <- fork $ actOnChan chan
    return $
        EventLoop
            { _addMsg = writeChan chan
            , _killEventLoop = killThread threadId
            }
  where
    actOnChan chan = do
        msg <- liftIO $ readChan chan
        handle msg
        actOnChan chan
