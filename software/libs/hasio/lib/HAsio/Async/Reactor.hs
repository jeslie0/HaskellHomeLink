{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
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

type EventHandler = ExceptT ErrorStack IO ()

type CallbackMap = Map.Map CInt (Map.Map Event EventHandler, [Flag])

-- * Class

class ReactorCore reactor where
  -- type EventPoller reactor :: Type
  type CallbackTableRef reactor :: Type
  type KeepRunningRef reactor :: Type

  getEventPoller :: reactor -> Epoll
  createEventPoller :: ExceptT ErrorStack IO Epoll

  getCallbackTableRef :: reactor -> CallbackTableRef reactor
  createCallbackTable :: IO (CallbackTableRef reactor)
  addCallback ::
    IsFd fd =>
    fd
    -> Event
    -> [Flag]
    -> EventHandler
    -> CallbackTableRef reactor
    -> IO ()
  deleteCallback :: IsFd fd => fd -> Event -> CallbackTableRef reactor -> IO ()
  findCallback ::
    IsFd fd => fd -> Event -> CallbackTableRef reactor -> IO (Maybe EventHandler)

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

  getRegisteredEvents :: IsFd fd => fd -> CallbackTableRef reactor -> IO ([Event], [Flag])

  cleanupReactor :: reactor -> ExceptT ErrorStack IO ()

-- * Types and instances

data Reactor = Reactor
  { epoll :: Epoll
  , callbackMapRef :: IORef CallbackMap
  , notifyStopFd :: EventFd
  , keepRunningRef :: IORef Bool
  }

instance ReactorCore Reactor where
  type CallbackTableRef Reactor = IORef CallbackMap
  type KeepRunningRef Reactor = IORef Bool

  getEventPoller = epoll
  createEventPoller = epollCreate1' 0

  getCallbackTableRef =
    callbackMapRef

  createCallbackTable =
    newIORef Map.empty

  addCallback fd event flags cb tableRef = do
    table <- readIORef tableRef
    writeIORef tableRef $
      Map.alter
        ( \case
            Nothing -> Just (Map.singleton event cb, flags)
            Just (eventMap, _) -> Just (Map.insert event cb eventMap, flags)
        )
        (fromIntegral . toFd $ fd)
        table

  deleteCallback fd event tableRef = do
    table <- readIORef tableRef
    writeIORef tableRef $
      Map.alter
        ( \case
            Nothing -> Nothing
            Just (eventMap, flags) ->
              let newMap = Map.alter (const Nothing) event eventMap
              in if null newMap then Nothing else Just (newMap, flags)
        )
        (fromIntegral . toFd $ fd)
        table

  findCallback fd event tableRef = do
    table <- readIORef tableRef
    case Map.lookup (fromIntegral . toFd $ fd) table of
      Nothing -> pure Nothing
      Just (eventMap, _) -> case Map.lookup event eventMap of
        Nothing -> pure Nothing
        Just cb -> pure $ Just cb

  getRegisteredEvents fd tableRef = do
    table <- readIORef tableRef
    case Map.lookup (fromIntegral . toFd $ fd) table of
      Nothing -> pure ([], [])
      Just (eventMap, flags) -> pure (Map.keys eventMap, flags)

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

  let notifyStopCB = do
        liftIO $ setKeepRunningRef @reactor keepRunningRef False
        void $ EventFd.read' notifyStopFd
  liftIO $
    addCallback @reactor notifyStopFd EpollIn [EpollET] notifyStopCB cbTableRef
  pure $ makeReactor eventPoller cbTableRef notifyStopFd keepRunningRef

registerFd ::
  forall reactor fd.
  (ReactorCore reactor, IsFd fd) =>
  reactor
  -> fd
  -> Event
  -> [Flag]
  -> EventHandler
  -> ExceptT ErrorStack IO ()
registerFd reactor fd event flags cb = do
  registeredEvents <-
    liftIO $ getRegisteredEvents @reactor fd (getCallbackTableRef reactor)
  case registeredEvents of
    ([], _) -> do
      liftIO $ print "epoll ctl add"
      epollCtl'
        (getEventPoller reactor)
        EpollCtlAdd
        fd
        (EpollEvent [event] flags (fromIntegral . toFd $ fd))
    (events,_) -> do
      liftIO $ print $ "epoll ctl modify: " <> (show (length events))
      epollCtl'
        (getEventPoller reactor)
        EpollCtlModify
        fd
        (EpollEvent (event : events) flags (fromIntegral . toFd $ fd))
  liftIO $ addCallback @reactor fd event flags cb (getCallbackTableRef reactor)

deregisterFd ::
  forall reactor fd.
  (ReactorCore reactor, IsFd fd) =>
  reactor
  -> fd
  -> Event
  -> ExceptT ErrorStack IO ()
deregisterFd reactor fd event = do
  liftIO $ deleteCallback @reactor fd event (getCallbackTableRef reactor)
  registeredEvents <-
    liftIO $ getRegisteredEvents @reactor fd (getCallbackTableRef reactor)
  case registeredEvents of
    ([], _) ->
      epollCtl'
        (getEventPoller reactor)
        EpollCtlDelete
        fd
        (EpollEvent [] [] (fromIntegral . toFd $ fd))
    (events, flags) ->
      epollCtl'
        (getEventPoller reactor)
        EpollCtlModify
        fd
        (EpollEvent events flags (fromIntegral . toFd $ fd))

run ::
  forall reactor. ReactorCore reactor => reactor -> ExceptT ErrorStack IO ()
run reactor = do
  liftIO $ setKeepRunningRef @reactor (getKeepRunningRef reactor) True
  go
 where
  go :: ExceptT ErrorStack IO ()
  go = do
    keepLooping <-
      liftIO . checkKeepRunningRef @reactor . getKeepRunningRef $ reactor
    when keepLooping $ do
      evs <- epollWait' (getEventPoller reactor) 64 (-1)
      forM_ evs $ \ev -> do
        let
          fd :: CInt = fromIntegral . dataRaw $ ev
          evss = events ev
        forM_ evss $ \e -> do
          mCb <- liftIO $ findCallback @reactor fd e (getCallbackTableRef reactor)
          case mCb of
            Nothing -> makeErrorStack AsyncErr.MissingCallback
            Just callback ->
              callback
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
