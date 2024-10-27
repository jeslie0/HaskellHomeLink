{-# LANGUAGE MonoLocalBinds #-}

module EventLoop (EventLoop(..), mkEventLoop) where

import Control.Concurrent (forkIO, killThread, newChan, readChan, writeChan)
import Handler (HasHandler, HandleableMsg (..), handle)
import Msg (Msg)

-- | A single threaded event loop that can have messages added to it
-- from another thread.
data EventLoop = EventLoop
  { addMsg :: forall msg. (HasHandler msg, Msg msg) => msg -> IO (),
    killEventLoop :: IO ()
  }

mkEventLoop :: IO EventLoop
mkEventLoop = do
  chan <- newChan @HandleableMsg
  threadId <- forkIO $ actOnChan chan
  return $
    EventLoop
      { addMsg = writeChan chan . HandleableMsg,
        killEventLoop = killThread threadId
      }
  where
    actOnChan chan = do
      (HandleableMsg msg) <- readChan chan
      handle msg
      actOnChan chan
