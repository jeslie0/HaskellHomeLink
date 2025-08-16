{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module HAsio.Async.Reactor (
  ReactorCore (..),
  Reactor (..),
  EventHandler,
  withReactor,
  cancel,
  run,
  deregisterFd,
  registerFd,
  mkReactor,
) where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Foreign.C.Types (CInt)
import HAsio.Async.Error qualified as AsyncErr
import HAsio.Error.ErrorStack (ErrorStack, makeErrorStack)
import HAsio.Fd (IsFd (..))
import HAsio.Fd.Epoll (
  Epoll,
  EpollCtlOp (..),
  EpollEvent (..),
  Event (..),
  Flag (..),
  epollCreate1',
  epollCtl',
  epollWait',
 )
import HAsio.Fd.EventFd as EventFd (
  EventFd,
  EventFdFlags (NonBlocking),
  eventFd',
  read',
  write',
 )
import HAsio.Fd.Syscalls (close')

type EventHandler = [Event] -> ExceptT ErrorStack IO ()

type CallbackMap = Map.Map CInt EventHandler

-- * Class

class ReactorCore reactor where
  -- type EventPoller reactor :: Type
  type CallbackTableRef reactor :: Type
  type KeepRunningRef reactor :: Type

  getEventPoller :: reactor -> Epoll
  createEventPoller :: ExceptT ErrorStack IO Epoll

  getCallbackTableRef ::
    reactor
    -> CallbackTableRef reactor
  createCallbackTable ::
    IO (CallbackTableRef reactor)
  addCallback ::
    IsFd fd =>
    fd
    -> EventHandler
    -> CallbackTableRef reactor
    -> IO ()
  deleteCallback :: IsFd fd => fd -> CallbackTableRef reactor -> IO ()
  findCallback ::
    IsFd fd => fd -> CallbackTableRef reactor -> IO (Maybe EventHandler)

  getKeepRunningRef :: reactor -> KeepRunningRef reactor
  createKeepRunningRef :: IO (KeepRunningRef reactor)
  setKeepRunningRef :: KeepRunningRef reactor -> Bool -> IO ()
  checkKeepRunningRef :: KeepRunningRef reactor -> IO Bool

  getNotifyStop :: reactor -> EventFd
  createNotifyStop :: ExceptT ErrorStack IO EventFd

  makeReactor ::
    Epoll
    -> CallbackTableRef reactor
    -> EventFd
    -> KeepRunningRef reactor
    -> reactor

  cleanupReactor :: reactor -> ExceptT ErrorStack IO ()

-- * Types and instances

data Reactor = Reactor
  { epoll :: Epoll
  , callbackMapRef :: IORef CallbackMap
  , notifyStopFd :: EventFd
  , keepRunningRef :: IORef Bool
  }

instance ReactorCore Reactor where
  type
    CallbackTableRef Reactor =
      IORef (Map.Map CInt EventHandler)
  type KeepRunningRef Reactor = IORef Bool

  getEventPoller = epoll
  createEventPoller = epollCreate1' 0

  getCallbackTableRef =
    callbackMapRef

  createCallbackTable =
    newIORef Map.empty

  addCallback fd cb tableRef = do
    table <- readIORef tableRef
    writeIORef tableRef $ Map.insert (fromIntegral . toFd $ fd) cb table

  deleteCallback fd tableRef = do
    table <- readIORef tableRef
    writeIORef tableRef $ Map.delete (fromIntegral . toFd $ fd) table

  findCallback fd tableRef = do
    table <- readIORef tableRef
    case Map.lookup (fromIntegral . toFd $ fd) table of
      Nothing -> pure Nothing
      Just cb -> pure $ Just cb

  getNotifyStop = notifyStopFd
  createNotifyStop = eventFd' 0 (Just EventFd.NonBlocking)

  getKeepRunningRef = keepRunningRef
  createKeepRunningRef = newIORef False
  setKeepRunningRef = writeIORef
  checkKeepRunningRef = readIORef

  makeReactor = Reactor

  cleanupReactor reactor = do
    close' (getEventPoller reactor)
    close' (notifyStopFd reactor)

-- data MTExecutor = MTExecutor
--   { mtEpoll :: Epoll
--   , mtCallbackMapRef :: IORef CallbackMap
--   , mtNotifyStopFd :: EventFd
--   , mtKeepRunningRef :: IORef Bool
--   }

-- * Functions

mkReactor ::
  forall reactor. ReactorCore reactor => ExceptT ErrorStack IO reactor
mkReactor = do
  eventPoller <- createEventPoller @reactor
  cbTableRef <- liftIO $ createCallbackTable @reactor
  notifyStopFd <- createNotifyStop @reactor
  keepRunningRef <- liftIO $ createKeepRunningRef @reactor

  let notifyStopCB events =
        case events of
          [EpollIn] -> do
            liftIO $ setKeepRunningRef @reactor keepRunningRef False
            void $ EventFd.read' notifyStopFd
          _ -> makeErrorStack AsyncErr.EventNotRegistered
  liftIO $ addCallback @reactor notifyStopFd notifyStopCB cbTableRef
  pure $ makeReactor eventPoller cbTableRef notifyStopFd keepRunningRef

registerFd ::
  forall reactor fd.
  (ReactorCore reactor, IsFd fd) =>
  reactor
  -> fd
  -> [Event]
  -> [Flag]
  -> EventHandler
  -> ExceptT ErrorStack IO ()
registerFd reactor fd events flags cb = do
  liftIO $ addCallback @reactor fd cb (getCallbackTableRef reactor)
  epollCtl'
    (getEventPoller reactor)
    EpollCtlAdd
    fd
    (EpollEvent events flags (fromIntegral . toFd $ fd))

deregisterFd ::
  forall reactor fd.
  (ReactorCore reactor, IsFd fd) =>
  reactor
  -> fd
  -> ExceptT ErrorStack IO ()
deregisterFd reactor fd = do
  liftIO $ deleteCallback @reactor fd (getCallbackTableRef reactor)
  epollCtl'
    (getEventPoller reactor)
    EpollCtlDelete
    fd
    (EpollEvent [] [] (fromIntegral . toFd $ fd))

run ::
  forall reactor. ReactorCore reactor => reactor -> ExceptT ErrorStack IO ()
run reactor = do
  liftIO $ setKeepRunningRef @reactor (getKeepRunningRef reactor) True
  liftIO $ print "RUNNING"
  go
 where
  go :: ExceptT ErrorStack IO ()
  go = do
    keepLooping <-
      liftIO . checkKeepRunningRef @reactor . getKeepRunningRef $ reactor
    when keepLooping $ do
      evs <- epollWait' (getEventPoller reactor) 64 (-1)
      forM_ evs $ \ev -> do
        let fd :: CInt = fromIntegral . dataRaw $ ev
        mCb <- liftIO $ findCallback @reactor fd (getCallbackTableRef reactor)
        case mCb of
          Nothing -> makeErrorStack AsyncErr.MissingCallback
          Just callback ->
            callback (events ev)
      go

cancel ::
  forall reactor. ReactorCore reactor => reactor -> ExceptT ErrorStack IO ()
cancel reactor = do
  liftIO $ setKeepRunningRef @reactor (getKeepRunningRef reactor) False
  EventFd.write' (getNotifyStop @reactor reactor) 1

withReactor ::
  forall reactor.
  ReactorCore reactor =>
  (reactor -> ExceptT ErrorStack IO ())
  -> ExceptT ErrorStack IO ()
withReactor action =
  ExceptT $ bracket aquire release use
 where
  aquire = do
    runExceptT $ mkReactor @reactor

  release (Left errs) = pure $ Left errs
  release (Right reactor) = runExceptT $ cleanupReactor @reactor reactor

  use (Left errs) = pure $ Left errs
  use (Right reactor) = runExceptT $ action reactor
