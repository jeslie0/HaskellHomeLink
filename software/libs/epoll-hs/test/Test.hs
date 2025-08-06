module Test where

import Control.Concurrent (forkIO, threadDelay)
import System.Clock (TimeSpec(..))
import System.Epoll (EpollCtlOp (..), epollCreate1, epollCtl, epollWait)
import System.Epoll.Internal (
  EpollEvent (..),
  EpollEventCode (..),
  epollEventCodeToWord,
 )
import System.EventFd qualified as Ev
import System.Posix.Types (Fd (..))
import System.TimerFd.Internal ( ITimerSpec(ITimerSpec) )
import System.TimerFd
    ( createTimerFd,
      ClockId(..),
      SetTimerFdFlags(..),
      ClockFlags(..),
      timerFdToFd,
      setTime)
import Control.Monad (forever)
import System.Posix.Internals (c_read)
import Foreign (alloca, Int64, Ptr, castPtr, Storable (..))
import System.Posix (getFdStatus)
import System.Posix.IO

main :: IO Int
main = do
  Just epoll <- epollCreate1 0
  Right eventfd <- createTimerFd Realtime [NonBlocking]
  fd <- timerFdToFd eventfd
  bool <- queryFdOption fd NonBlockingRead
  print $ "fd is non blocking: " <> show bool
  Right _ <- setTime eventfd [] (ITimerSpec (TimeSpec 5 0) (TimeSpec 5 0))
  let event = EpollEvent (epollEventCodeToWord EpollIn) 78
  Right () <- epollCtl epoll EpollCtlAdd fd event
  putStrLn "Waiting for input on event fd..."
  forever $ do
    Just [EpollEvent fd' data'] <- epollWait epoll 10 (-1)
    val <- alloca $ \(ptr :: Ptr Int64) -> do
      print "Event occurred!"
      c_read (fromIntegral fd) (castPtr ptr) 8 >> peek ptr
    putStrLn $ "Read " <> show val <> " from timerfd"
  pure 0
