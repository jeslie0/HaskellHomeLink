module Threads (
    AsyncComputation,
    spawnAsyncComputation,
    isAsyncComputationRunning,
    killAsyncComputation,
) where

import Control.Concurrent (
    MVar,
    ThreadId,
    forkFinally,
    isEmptyMVar,
    killThread,
    newEmptyMVar,
    putMVar,
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
spawnAsyncComputation task = do
    isRunningMVar <- newEmptyMVar
    threadId <- forkFinally task $ \_ -> void $ putMVar isRunningMVar ()
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
killAsyncComputation (AsyncComputation threadId _) = do
    killThread threadId
