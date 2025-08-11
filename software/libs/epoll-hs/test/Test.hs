module Test where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Foreign (Int64, Ptr, Storable (..), alloca, castPtr)
import HIO.Fd (IsFd (..))
import HIO.Fd.Epoll (EpollCtlOp (EpollCtlAdd), EpollEvent (..), Event (..), Flag (EpollET), epollCreate1_, epollCtl_, epollWait_)
import HIO.Fd.TimerFd (ClockId (..), createTimerFd_)
import HIO.Fd.TimerFd qualified as TFD
import System.Clock (TimeSpec (..))
import System.Posix (getFdStatus)
import System.Posix.Internals (c_read)
import System.Posix.Types (Fd (..))

main :: IO Int
main = do
  epoll <- epollCreate1_ 0
  eventfd <- createTimerFd_ Realtime [TFD.NonBlocking]
  let fd = toFd eventfd
  _ <- TFD.setTime_ eventfd [] (TFD.ITimerSpec (TimeSpec 5 0) (TimeSpec 5 0))
  let event = EpollEvent [EpollIn] [EpollET] 78
  _ <- epollCtl_ epoll EpollCtlAdd fd event
  putStrLn "Waiting for input on event fd..."
  forever $ do
    [EpollEvent fd' _ data'] <- epollWait_ epoll 10 (-1)
    val <- alloca $ \(ptr :: Ptr Int64) -> do
      print "Event occurred!"
      c_read (fromIntegral fd) (castPtr ptr) 8 >> peek ptr
    putStrLn $ "Read " <> show val <> " from timerfd"
  pure 0
