module Threads (
    AsyncComputation,
    spawnAsyncComputation,
    spawnAsyncComputationWithNotify,
    isAsyncComputationRunning,
    killAsyncComputation,
) where

import Control.Concurrent (
    MVar,
    ThreadId,
    forkIO,
    isEmptyMVar,
    killThread,
    newEmptyMVar,
    tryPutMVar,
 )
import Control.Monad (void)

-- | A task running in another thread.
data AsyncComputation = AsyncComputation
    { threadId :: ThreadId
    , isRunningMVar :: MVar ()
    }

-- | Run a task in another thread.
spawnAsyncComputation ::
    IO () -> IO AsyncComputation
spawnAsyncComputation task =
    spawnAsyncComputationWithNotify task (pure ())

{- | Run a task in another thread, giving a function which will be
called when the task is completed.
-}
spawnAsyncComputationWithNotify ::
    IO () -> IO () -> IO AsyncComputation
spawnAsyncComputationWithNotify task onFinish = do
    isRunningMVar <- newEmptyMVar
    threadId <- forkIO $ task >> tryPutMVar isRunningMVar () >> onFinish
    pure $
        AsyncComputation
            { threadId
            , isRunningMVar
            }

-- | Check to see if an asynchronous computation is running.
isAsyncComputationRunning :: AsyncComputation -> IO Bool
isAsyncComputationRunning (AsyncComputation _ isRunningMVar) = do
    isEmptyMVar isRunningMVar

-- | Kill an asynchronous computation.
killAsyncComputation :: AsyncComputation -> IO ()
killAsyncComputation (AsyncComputation threadId isRunningMVar) = do
    killThread threadId
    void $ tryPutMVar isRunningMVar ()
