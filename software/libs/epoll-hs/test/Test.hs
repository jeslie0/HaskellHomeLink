module Test where
import System.Epoll ( epollCreate1, epollCtl, EpollCtlOp(..), epollWait)
import System.Epoll.Internal (EpollEvent(..), EpollEventCode(..), epollEventCodeToWord)
import System.Posix.Types (Fd(..))


main :: IO Int
main = do
  Just epoll <- epollCreate1 0
  let event = EpollEvent (epollEventCodeToWord EpollIn) 78
  Right () <- epollCtl epoll EpollCtlAdd (Fd 0) event
  putStrLn "Waiting for input on stdin..."
  Just [EpollEvent fd data'] <- epollWait epoll 10 (-1)
  putStrLn $ "Something happened to fd " <> show fd <> " with data " <> show data'
  pure 0
