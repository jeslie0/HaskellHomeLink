module HIO.Async.IOContext (IOContext (..), mkIOContext) where

import Control.Monad (forM_, void, when)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import HIO.Async.Error qualified as AsyncErr
import HIO.Error.ErrorStack (ErrorStack, makeErrorStack)
import HIO.Fd (IsFd (..))
import HIO.Fd.Epoll (
  EpollCtlOp (..),
  EpollEvent (..),
  Event (..),
  Flag (..),
  epollCreate1',
  epollCtl',
  epollWait',
 )
import HIO.Fd.EventFd (EventFdFlags (NonBlocking), eventFd', read', write')
import System.Posix (Fd (..))

data IOContext = IOContext
  { register ::
      forall fd.
      IsFd fd =>
      fd
      -> [Event]
      -> [Flag]
      -> ([Event] -> ExceptT ErrorStack IO ())
      -> ExceptT ErrorStack IO ()
  , deregister :: forall fd. IsFd fd => fd -> ExceptT ErrorStack IO ()
  , run :: ExceptT ErrorStack IO ()
  , cancel :: ExceptT ErrorStack IO ()
  }

mkIOContext :: ExceptT ErrorStack IO IOContext
mkIOContext = do
  epoll <- epollCreate1' 0
  keepLoopingEvFd <- eventFd' 0 (Just NonBlocking)
  keepLoopingRef <- liftIO $ newIORef False

  callbackMapRef <-
    liftIO . newIORef $
      Map.singleton
        (fromIntegral $ toFd keepLoopingEvFd)
        (\_ -> void $ HIO.Fd.EventFd.read' keepLoopingEvFd)

  epollCtl'
    epoll
    EpollCtlAdd
    (toFd keepLoopingEvFd)
    (EpollEvent [EpollIn] [EpollET] (fromIntegral (toFd keepLoopingEvFd)))

  let
    register res events flags cb = do
      let Fd fd = toFd res
      liftIO $ modifyIORef' callbackMapRef $ Map.insert fd cb
      epollCtl'
        epoll
        EpollCtlAdd
        (Fd fd)
        (EpollEvent events flags (fromIntegral fd))

    deregister res = do
      let Fd fd = toFd res
      liftIO $ modifyIORef' callbackMapRef $ Map.delete fd
      epollCtl'
        epoll
        EpollCtlDelete
        (Fd fd)
        (EpollEvent [] [] (fromIntegral fd))

    cancel = do
      liftIO $ writeIORef keepLoopingRef False
      write' keepLoopingEvFd 1

    run = do
      liftIO $ writeIORef keepLoopingRef True
      go
     where
      go :: ExceptT ErrorStack IO ()
      go = do
        keepLooping <- liftIO $ readIORef keepLoopingRef
        when keepLooping $ do
          evs <- epollWait' epoll 64 (-1)
          forM_ evs $ \ev -> do
            let fd = fromIntegral . dataRaw $ ev
            cbMap <- liftIO $ readIORef callbackMapRef
            case Map.lookup fd cbMap of
              Nothing -> makeErrorStack AsyncErr.MissingCallback
              Just callback ->
                callback (events ev)
          go

  pure $ IOContext {register, deregister, run, cancel}
