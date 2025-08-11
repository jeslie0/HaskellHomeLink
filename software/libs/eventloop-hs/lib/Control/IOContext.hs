{-# LANGUAGE TemplateHaskell #-}

module Control.IOContext (
  ToFd (..),
  SomeToFd (..),
  IOContext (..),
  register,
  deregister,
  run,
  cancel,
  mkIOContext,
) where

import Control.Exception (IOException)
import Control.Monad (void, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Lens.Micro.TH (makeLenses)
import Network.Socket (Socket, unsafeFdSocket)
import System.Epoll (
  Epoll,
  EpollCtlOp (..),
  EpollEvent (..),
  Event (..),
  Flag (..),
  epollCreate1',
  epollCtl',
  epollCtl_,
  epollToFd,
  epollWait,
 )
import System.EventFd (
  EventFd,
  EventFdFlags (NonBlocking),
  eventFd',
  eventFdToFd,
  read_,
  write_,
 )
import System.IO (Handle)
import System.Posix.IO (handleToFd)
import System.Posix.Types (Fd (..))
import System.TimerFd (TimerFd, timerFdToFd)

class ToFd a where
  toFd :: a -> IO Fd

instance ToFd Fd where
  toFd = pure

instance ToFd Socket where
  toFd sock = Fd <$> unsafeFdSocket sock

instance ToFd Handle where
  toFd = handleToFd

instance ToFd Epoll where
  toFd = epollToFd

instance ToFd EventFd where
  toFd = eventFdToFd

instance ToFd TimerFd where
  toFd = timerFdToFd

data SomeToFd = forall a. ToFd a => SomeToFd a

instance ToFd SomeToFd where
  toFd (SomeToFd a) = toFd a

data IOContext = IOContext
  { _register :: ToFd fd => (fd, [Event] -> IO ()) -> [Event] -> [Flag] -> IO ()
  , _deregister :: ToFd fd => fd -> IO ()
  , _run :: IO ()
  , _cancel :: IO ()
  }

$(makeLenses ''IOContext)

mkIOContext :: IO (Either IOException IOContext)
mkIOContext =
  runExceptT $ do
    epoll <- epollCreate1' 0
    keepLoopingEvFd <- eventFd' 0 (Just NonBlocking)
    keepLoopingFd <- liftIO $ toFd keepLoopingEvFd
    keepLoopingRef <- liftIO $ newIORef False

    callbackMapRef <-
      liftIO . newIORef $
        Map.singleton
          (fromIntegral keepLoopingFd)
          (\_ -> void $ System.EventFd.read_ keepLoopingEvFd)

    epollCtl'
      epoll
      EpollCtlAdd
      keepLoopingFd
      (EpollEvent [EpollIn] [EpollET] (fromIntegral keepLoopingFd))

    let
      _register (res, cb) events flags = do
        Fd fd <- toFd res
        modifyIORef' callbackMapRef $ Map.insert fd cb
        epollCtl_
          epoll
          EpollCtlAdd
          (Fd fd)
          (EpollEvent events flags (fromIntegral fd))

      _deregister (SomeToFd res) = do
        Fd fd <- toFd res
        modifyIORef' callbackMapRef $ Map.delete fd
        epollCtl_
          epoll
          EpollCtlDelete
          (Fd fd)
          (EpollEvent [] [] (fromIntegral fd))

      _cancel = do
        writeIORef keepLoopingRef False
        write_ keepLoopingEvFd 1

      _run = do
        writeIORef keepLoopingRef True
        go
       where
        go = do
          keepLooping <- readIORef keepLoopingRef
          when keepLooping $ do
            eEvents <- epollWait epoll 64 (-1)
            case eEvents of
              Left err -> print err
              Right evs -> do
                forM_ evs $ \ev -> do
                  let fd = fromIntegral . dataRaw $ ev
                  cbMap <- readIORef callbackMapRef
                  case Map.lookup fd cbMap of
                    Nothing -> putStrLn "No callback found for fd"
                    Just callback ->
                      callback (events ev)

                go

    pure $ IOContext {_register, _deregister, _run, _cancel}
