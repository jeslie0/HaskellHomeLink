module Test where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Foreign (Int64, Ptr, Storable (..), alloca, castPtr)
import HIO.Async.IOContext (IOContext (..), mkIOContext)
import HIO.Fd (IsFd (..))
import HIO.Fd.Epoll (
  EpollCtlOp (EpollCtlAdd),
  EpollEvent (..),
  Event (..),
  Flag (EpollET),
  epollCreate1',
  epollCtl',
  epollWait',
 )
import HIO.Fd.Syscalls (readUnsafe)
import HIO.Fd.TimerFd qualified as TFD
import System.Posix (getFdStatus)
import System.Posix.Internals (c_read)
import System.Posix.Types (Fd (..))

main :: IO Int
main = do
  result <- go
  case result of
    Left errs -> print errs >> pure (-1)
    Right _ -> pure 0
 where
  go = runExceptT $ do
    ctx <- mkIOContext
    timerfd <- TFD.createTimerFd' TFD.Realtime [TFD.NonBlocking]
    TFD.setTime' timerfd [] (TFD.ITimerSpec (TFD.TimeSpec 5 0) (TFD.TimeSpec 5 0))
    let event = EpollEvent [EpollIn] [EpollET] 78
    register ctx timerfd [EpollIn] [EpollET] $ \ev -> do
      val <- liftIO $ alloca $ \(ptr :: Ptr Int64) -> do
        print "Event occurred!"
        readUnsafe timerfd (castPtr ptr) 8 >> peek ptr
      liftIO . putStrLn $ "Read " <> show val <> " from timerfd"

    liftIO $ putStrLn "Waiting for input on event fd..."
    run ctx
