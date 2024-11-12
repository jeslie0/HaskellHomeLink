module ThreadPool (ThreadPool, addTask, mkThreadPool, killThreadPool) where

import Control.Concurrent (
    MVar,
    ThreadId,
    forkIO,
    killThread,
    newMVar,
    putMVar,
    takeMVar, forkIOWithUnmask,
 )
import Control.Exception (bracket_, Exception, mask_, catch)
import Data.Foldable (forM_, for_)
import Data.Mutable (Deque, newColl, popFront, pushBack)
import Data.Vector.Mutable qualified as V

data ThreadPool = ThreadPool
    { _tasksMVar :: MVar (Deque V.MVector V.RealWorld (IO ()))
    , _maxThreadCount :: {-# UNPACK #-} !Int
    , _threadPool :: MVar ([ThreadId], Int)
    , _activeThreadCount :: MVar Int
    }

addTask :: Exception e => ThreadPool -> IO () -> (e -> IO ()) -> IO ()
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
        threadCount <- takeMVar _activeThreadCount
        let !newThreadCount = threadCount + 1
        putMVar _activeThreadCount newThreadCount

    decrementActiveThreadCount = do
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
