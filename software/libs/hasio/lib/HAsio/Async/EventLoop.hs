{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HAsio.Async.EventLoop (
  STEventLoop,
  withEventLoop,
  addMsg,
  cancel,
  run,
  setInterval,
  setTimeout,
  killInterval,
  killTimeout,
  getReactor,
) where

import Control.Monad (forM_, void)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Mutable qualified as D
import Data.Typeable (Typeable)
import Foreign (Ptr, Storable, alloca)
import HAsio.Async.Reactor (Reactor, deregisterFd, registerFd)
import HAsio.Async.Reactor qualified as Reactor
import HAsio.Error.Error (Error (..))
import HAsio.Error.ErrorCategory (ErrorCategory (..))
import HAsio.Error.ErrorStack (ErrorStack, makeErrorStack)
import HAsio.Fd.Epoll (Event (EpollIn), Flag (EpollET))
import HAsio.Fd.EventFd (EventFd)
import HAsio.Fd.EventFd qualified as EventFd
import HAsio.Fd.Syscalls qualified as Fd
import HAsio.Fd.TimerFd (ITimerSpec (..), TimeSpec (..), TimerFd)
import HAsio.Fd.TimerFd qualified as TimerFd
import System.Random (StdGen, getStdGen, uniform)

newtype TimeoutId = TimeoutId Int deriving (Ord, Eq, Show)

newtype IntervalId = IntervalId Int deriving (Ord, Eq, Show)

-- class EventLoop loop where
--   type EventLoopMsg loop :: Type

--   addMsg :: EventLoopMsg loop -> loop -> ExceptT ErrorStack IO ()

--   setInterval :: EventLoopMsg loop -> Int -> loop -> ExceptT ErrorStack IO ()
--   killInterval :: IntervalId -> loop -> ExceptT ErrorStack IO ()

--   setTimeout :: EventLoopMsg loop -> Int -> loop -> ExceptT ErrorStack IO ()
--   killTimeout :: TimeoutId -> loop -> ExceptT ErrorStack IO ()

--   run :: EventLoopMsg loop -> ExceptT ErrorStack IO ()
--   cancel :: EventLoopMsg loop -> ExceptT ErrorStack IO ()

data STEventLoop msg = STEventLoop
  { ctx :: Reactor
  , eventFd :: EventFd
  , waitingEvents :: D.BDeque D.RealWorld msg
  , timeoutMapRef :: IORef (Map.Map TimeoutId TimerFd)
  , intervalMapRef :: IORef (Map.Map IntervalId TimerFd)
  , genRef :: IORef StdGen
  }

data STEventLoopError
  = TimerFdNotFound
  | IntervalFdNotFound
  | EventsQueueEmpty
  deriving (Typeable)

data EventLoopErrorCategory

instance Error STEventLoopError where
  type ECat STEventLoopError = EventLoopErrorCategory

  getErrorMessage err =
    case err of
      TimerFdNotFound -> "Could not find timerfd corresponding to timeoutId"
      IntervalFdNotFound -> "Could not find intervalfd corresponding to intervalId"
      EventsQueueEmpty -> "Could not extract message from events queue"

instance ErrorCategory EventLoopErrorCategory where
  getErrorCategoryName = "eventloop"

addMsg :: msg -> STEventLoop msg -> ExceptT ErrorStack IO ()
addMsg msg (STEventLoop _ eventFd waitingEvents _ _ _) = do
  D.pushBack waitingEvents msg
  EventFd.write' eventFd 1

alloca ::
  Storable a => (Ptr a -> ExceptT ErrorStack IO b) -> ExceptT ErrorStack IO b
alloca func = do
  eRes <- liftIO $ Foreign.alloca (runExceptT . func)
  ExceptT . pure $ eRes

setInterval :: STEventLoop msg -> msg -> Int -> ExceptT ErrorStack IO IntervalId
setInterval loop@(STEventLoop ctx _ _ _ intervalMapRef genRef) msg ms = do
  let
    seconds = fromIntegral $ ms `div` 1000
    nanos = fromIntegral $ (ms `mod` 1000) * 1000
    tSpec = TimeSpec seconds nanos
    itSpec = ITimerSpec {it_value = tSpec, it_interval = tSpec}

  timerfd <- TimerFd.createTimerFd' TimerFd.Monotonic [TimerFd.NonBlocking]
  TimerFd.setTime'
    timerfd
    [TimerFd.RelativeTime]
    itSpec

  registerFd ctx timerfd EpollIn [EpollET] $ do
    -- Read the data and discard it...
    void . HAsio.Async.EventLoop.alloca $ \ptr -> Fd.readUnsafe' @Int timerfd ptr 8
    addMsg msg loop

  gen <- liftIO $ readIORef genRef
  let
    (!n :: Int, !newGen) = uniform gen
    intervalId = IntervalId n
  liftIO $ writeIORef genRef newGen

  intervalMap <- liftIO $ readIORef intervalMapRef
  liftIO . writeIORef intervalMapRef $ Map.insert intervalId timerfd intervalMap
  pure intervalId

killInterval :: IntervalId -> STEventLoop msg -> ExceptT ErrorStack IO ()
killInterval intervalId (STEventLoop ctx _ _ _ intervalMapRef _) = do
  intervalMap <- liftIO . readIORef $ intervalMapRef
  case Map.lookup intervalId intervalMap of
    Nothing -> makeErrorStack TimerFdNotFound
    Just timerfd -> do
      Fd.closeUnsafe' timerfd
      deregisterFd ctx timerfd EpollIn
      liftIO . writeIORef intervalMapRef $ Map.delete intervalId intervalMap

setTimeout :: STEventLoop msg -> msg -> Int -> ExceptT ErrorStack IO TimeoutId
setTimeout loop@(STEventLoop ctx _ _ timeoutMapRef _ genRef) msg ms = do
  let
    seconds = fromIntegral $ ms `div` 1000
    nanos = fromIntegral $ (ms `mod` 1000) * 1000
    tSpec = TimeSpec seconds nanos
    itSpec = ITimerSpec {it_value = tSpec, it_interval = TimeSpec 0 0}

  timerfd <- TimerFd.createTimerFd' TimerFd.Monotonic [TimerFd.NonBlocking]
  TimerFd.setTime'
    timerfd
    [TimerFd.RelativeTime]
    itSpec

  gen <- liftIO $ readIORef genRef
  let
    (!n :: Int, !newGen) = uniform gen
    timeoutId = TimeoutId n
  liftIO $ writeIORef genRef newGen

  registerFd ctx timerfd EpollIn [EpollET] $ do
    -- Read the data and discard it...
    void . HAsio.Async.EventLoop.alloca $ \ptr -> Fd.readUnsafe' @Int timerfd ptr 8
    deregisterFd ctx timerfd EpollIn
    timeoutMap <- liftIO $ readIORef timeoutMapRef
    liftIO . writeIORef timeoutMapRef $ Map.delete timeoutId timeoutMap
    addMsg msg loop

  intervalMap <- liftIO $ readIORef timeoutMapRef
  liftIO . writeIORef timeoutMapRef $ Map.insert timeoutId timerfd intervalMap
  pure timeoutId

killTimeout :: TimeoutId -> STEventLoop msg -> ExceptT ErrorStack IO ()
killTimeout timeoutId (STEventLoop ctx _ _ timeoutMapRef _ _) = do
  intervalMap <- liftIO . readIORef $ timeoutMapRef
  case Map.lookup timeoutId intervalMap of
    Nothing -> makeErrorStack TimerFdNotFound
    Just timerfd -> do
      Fd.closeUnsafe' timerfd
      deregisterFd ctx timerfd EpollIn
      liftIO . writeIORef timeoutMapRef $ Map.delete timeoutId intervalMap

withEventLoop ::
  (STEventLoop msg -> msg -> ExceptT ErrorStack IO ())
  -> (STEventLoop msg -> ExceptT ErrorStack IO ())
  -> ExceptT ErrorStack IO ()
withEventLoop handleEvent withLoop = do
  waitingEvents <- liftIO D.newColl
  timeoutMapRef <- liftIO $ newIORef Map.empty
  intervalMapRef <- liftIO $ newIORef Map.empty
  genRef <- getStdGen >>= liftIO . newIORef
  Reactor.withReactor $ \ctx -> do
    eventFd <- EventFd.eventFd' 0 [EventFd.NonBlocking]

    let loop =
          STEventLoop
            { ctx = ctx
            , eventFd = eventFd
            , waitingEvents = waitingEvents
            , timeoutMapRef = timeoutMapRef
            , intervalMapRef = intervalMapRef
            , genRef = genRef
            }

    registerFd ctx eventFd EpollIn [EpollET] $ do
      n <- EventFd.read' eventFd
      forM_ [1 .. n] $ \_ -> do
        mMsg <- liftIO $ D.popFront waitingEvents
        case mMsg of
          Nothing -> makeErrorStack EventsQueueEmpty
          Just msg -> handleEvent loop msg

    withLoop loop

run :: STEventLoop msg -> ExceptT ErrorStack IO ()
run loop = do
  Reactor.run (ctx loop)

cancel :: STEventLoop msg -> ExceptT ErrorStack IO ()
cancel loop = do
  Reactor.cancel (ctx loop)

getReactor :: STEventLoop msg -> Reactor
getReactor = ctx
