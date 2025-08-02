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

main :: IO Int
main = do
  Just epoll <- epollCreate1 0
  Right eventfd <- createTimerFd Realtime [NonBlocking]
  fd <- timerFdToFd eventfd
  Right _ <- setTime eventfd [] (ITimerSpec (TimeSpec 5 5) (TimeSpec 5 0))
  let event = EpollEvent (epollEventCodeToWord EpollIn) 78
  Right () <- epollCtl epoll EpollCtlAdd fd event
  putStrLn "Waiting for input on event fd..."
  Just [EpollEvent fd data'] <- epollWait epoll 10 (-1)
  putStrLn $ "Something happened to fd " <> show fd <> " with data " <> show data'
  pure 0
