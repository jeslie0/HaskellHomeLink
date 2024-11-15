module ThreadPool (
    ThreadPool,
    addTask,
    addTaskUnmasked,
    mkThreadPool,
    killThreadPool,
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
    forkIOWithUnmask,
    isEmptyMVar,
    killThread,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    tryPutMVar,
 )
import Control.Exception (
    Exception,
    bracket_,
    catch,
    mask_,
 )
import Control.Monad (void)
import Data.Foldable (forM_, for_)
import Data.Mutable (Deque, newColl, popFront, pushBack)
import Data.Vector.Mutable qualified as V

data ThreadPool = ThreadPool
    { _tasksMVar :: MVar (Deque V.MVector V.RealWorld (IO ()))
    , _maxThreadCount :: {-# UNPACK #-} !Int
    , _threadPool :: MVar ([ThreadId], Int)
    , _activeThreadCount :: MVar Int
    }

addTaskUnmasked :: ThreadPool -> IO () -> IO ()
addTaskUnmasked pool@(ThreadPool _tasksMVar _maxThreadCount _threadPool _activeThreadCount) task = do
    activeThreadCount <- takeMVar _activeThreadCount
    threadPool@(currentThreads, currentThreadsSize) <- takeMVar _threadPool

    -- Extend the thread pool if required.
    if currentThreadsSize < _maxThreadCount && activeThreadCount < _maxThreadCount
        then do
            newThread <- forkIO $ mainThread pool
            let !newLen = currentThreadsSize + 1
            putMVar _threadPool (newThread : currentThreads, newLen)
        else putMVar _threadPool threadPool
    putMVar _activeThreadCount activeThreadCount

    deque <- takeMVar _tasksMVar
    pushBack deque task
    putMVar _tasksMVar deque

addTask :: (Exception e) => ThreadPool -> IO () -> (e -> IO ()) -> IO ()
addTask pool@(ThreadPool _tasksMVar _maxThreadCount _threadPool _activeThreadCount) task handler = do
    activeThreadCount <- takeMVar _activeThreadCount
    threadPool@(currentThreads, currentThreadsSize) <- takeMVar _threadPool

    -- Extend the thread pool if required.
    if currentThreadsSize < _maxThreadCount && activeThreadCount < _maxThreadCount
        then do
            newThread <- mask_ $ forkIOWithUnmask $ \unmask -> catch (unmask $ mainThread pool) handler
            let !newLen = currentThreadsSize + 1
            putMVar _threadPool (newThread : currentThreads, newLen)
        else putMVar _threadPool threadPool
    putMVar _activeThreadCount activeThreadCount

    deque <- takeMVar _tasksMVar
    pushBack deque task
    putMVar _tasksMVar deque

mainThread :: ThreadPool -> IO ()
mainThread pool@(ThreadPool _tasksMVar _ _ _activeThreadCount) = do
    deque <- takeMVar _tasksMVar
    mTask <- popFront deque
    putMVar _tasksMVar deque
    for_ mTask $ bracket_ incrementActiveThreadCount decrementActiveThreadCount
    mainThread pool
  where
    incrementActiveThreadCount = do
        putStrLn "Increment"
        threadCount <- takeMVar _activeThreadCount
        let !newThreadCount = threadCount + 1
        putMVar _activeThreadCount newThreadCount

    decrementActiveThreadCount = do
        putStrLn "Decrement"
        threadCount <- takeMVar _activeThreadCount
        let !newThreadCount = threadCount - 1
        putMVar _activeThreadCount newThreadCount

mkThreadPool :: Int -> IO ThreadPool
mkThreadPool n = do
    emptyDeque <- newColl @(Deque V.MVector V.RealWorld (IO ()))
    _tasksMVar <- newMVar emptyDeque

    _threadPool <- newMVar ([], 0 :: Int)
    _activeThreadCount <- newMVar (0 :: Int)
    return $
        ThreadPool
            { _tasksMVar
            , _maxThreadCount = n
            , _threadPool
            , _activeThreadCount
            }

killThreadPool :: ThreadPool -> IO ()
killThreadPool (ThreadPool _tasksMVar _ _threadPool _activeThreadCount) = do
    emptyDeque <- newColl @(Deque V.MVector V.RealWorld (IO ()))

    (threads, _) <- takeMVar _threadPool
    forM_ threads killThread

    putMVar _tasksMVar emptyDeque
    putMVar _threadPool ([], 0)
    putMVar _activeThreadCount 0

data AsyncComputation = AsyncComputation
    { threadId :: ThreadId
    , isRunningMVar :: MVar ()
    }

spawnAsyncComputation ::
    IO () -> IO AsyncComputation
spawnAsyncComputation task =
    spawnAsyncComputationWithNotify task (pure ())

spawnAsyncComputationWithNotify ::
    IO () -> IO () -> IO AsyncComputation
spawnAsyncComputationWithNotify task onFinish = do
    isRunningMVar <- newEmptyMVar
    threadId <- forkIO $ task >> tryPutMVar isRunningMVar () >> onFinish
    -- threadId <- mask_ $ forkIOWithUnmask $ \restore -> do
    --     catch (restore task >> tryPutMVar isRunningMVar () >> onFinish) handler
    pure $
        AsyncComputation
            { threadId
            , isRunningMVar
            }

isAsyncComputationRunning :: AsyncComputation -> IO Bool
isAsyncComputationRunning (AsyncComputation _ isRunningMVar) = do
    isEmptyMVar isRunningMVar

killAsyncComputation :: AsyncComputation -> IO ()
killAsyncComputation (AsyncComputation threadId isRunningMVar) = do
    killThread threadId
    void $ tryPutMVar isRunningMVar ()
