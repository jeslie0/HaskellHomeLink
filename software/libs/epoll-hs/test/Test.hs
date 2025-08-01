module Test where

import Control.Concurrent (forkIO, threadDelay)
import System.Epoll (EpollCtlOp (..), epollCreate1, epollCtl, epollWait)
import System.Epoll.Internal (
  EpollEvent (..),
  EpollEventCode (..),
  epollEventCodeToWord,
 )
import System.EventFd (EventFdFlags (NonBlocking), eventFd, eventFdToFd, write)
import System.EventFd qualified as Ev
import System.Posix.Types (Fd (..))

main :: IO Int
main = do
  Just epoll <- epollCreate1 0
  Right eventfd <- eventFd 0 [NonBlocking]
  fd <- eventFdToFd eventfd
  let event = EpollEvent (epollEventCodeToWord EpollIn) 78
  Right () <- epollCtl epoll EpollCtlAdd fd event
  putStrLn "Waiting for input on event fd..."
  forkIO $ do
    threadDelay 5000000
    write eventfd 1
    pure ()
  Just [EpollEvent fd data'] <- epollWait epoll 10 (-1)
  putStrLn $ "Something happened to fd " <> show fd <> " with data " <> show data'
  Right n <- Ev.read eventfd
  putStrLn $ "Got : " <> show n
  pure 0
