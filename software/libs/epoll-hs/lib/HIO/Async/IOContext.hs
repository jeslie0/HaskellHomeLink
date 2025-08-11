module HIO.Async.IOContext (IOContext(..)) where

import HIO.Fd (IsFd (..))
import HIO.Fd.Epoll (Event, Flag, epollCreate1')
import Foreign.C (Errno)
import System.Posix (Fd(..))
import Control.Monad.Except (runExceptT)

data IOContext = IOContext
  { register ::
      forall fd.
      IsFd fd =>
      (fd, [Event] -> IO ())
      -> [Event]
      -> [Flag]
      -> IO ()
  , deregister :: forall fd. IsFd fd => fd -> IO ()
  , run :: IO ()
  , cancel :: IO ()
  }

-- mkIOContext :: IO (Either Errno IOContext)
-- mkIOContext =
--   runExceptT $ do
--     epoll <- epollCreate1' 0
--     keepLoopingEvFd <- eventFd' 0 (Just NonBlocking)
--     keepLoopingFd <- liftIO $ toFd keepLoopingEvFd
--     keepLoopingRef <- liftIO $ newIORef False

--     callbackMapRef <-
--       liftIO . newIORef $
--         Map.singleton
--           (fromIntegral keepLoopingFd)
--           (\_ -> void $ System.EventFd.read_ keepLoopingEvFd)

--     epollCtl'
--       epoll
--       EpollCtlAdd
--       keepLoopingFd
--       (EpollEvent [EpollIn] [EpollET] (fromIntegral keepLoopingFd))

--     let
--       register (res, cb) events flags = do
--         Fd fd <- toFd res
--         modifyIORef' callbackMapRef $ Map.insert fd cb
--         epollCtl_
--           epoll
--           EpollCtlAdd
--           (Fd fd)
--           (EpollEvent events flags (fromIntegral fd))

--       deregister res = do
--         Fd fd <- toFd res
--         modifyIORef' callbackMapRef $ Map.delete fd
--         epollCtl_
--           epoll
--           EpollCtlDelete
--           (Fd fd)
--           (EpollEvent [] [] (fromIntegral fd))

--       cancel = do
--         writeIORef keepLoopingRef False
--         write_ keepLoopingEvFd 1

--       run = do
--         writeIORef keepLoopingRef True
--         go
--        where
--         go = do
--           keepLooping <- readIORef keepLoopingRef
--           when keepLooping $ do
--             eEvents <- epollWait epoll 64 (-1)
--             case eEvents of
--               Left err -> print err
--               Right evs -> do
--                 forM_ evs $ \ev -> do
--                   let fd = fromIntegral . dataRaw $ ev
--                   cbMap <- readIORef callbackMapRef
--                   case Map.lookup fd cbMap of
--                     Nothing -> putStrLn "No callback found for fd"
--                     Just callback ->
--                       callback (events ev)

--                 go

--     pure $ IOContext {register, deregister, run, cancel}
