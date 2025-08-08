module Test where

import Control.IOContext (ResourceCallback (..), ToFd (..), mkIOContext, _cancel, _register, _run)
import Data.ByteString qualified as B
import Foreign (allocaArray, castPtr)
import Foreign.C (Errno (Errno), getErrno)
import GHC.IO.Handle.FD (fdToHandle)
import System.Epoll (Event (..), Flag (..))
import System.IO (
  BufferMode (NoBuffering),
  hGetContents,
  hGetLine,
  hSetBuffering,
  stdin,
 )
import System.Posix.ByteString (fdRead, fdReadBuf)

main :: IO Int
main = do
  Right ctx <- mkIOContext
  fd <- toFd stdin -- this detaches the Handle from the runtime!
  _register
    ctx
    ( ResourceCallback
        fd
        ( \handle ev -> case ev of
            EpollIn -> do
              putStr "Input to stdin detected: "
              result <- allocaArray 1024 $ \ptr -> do
                bytesRead <- fdReadBuf fd ptr 1024
                if bytesRead > 0
                  then Right <$> B.packCStringLen (castPtr ptr, fromIntegral bytesRead)
                  else Left <$> getErrno
              case result of
                Left (Errno err) -> putStrLn $ "error!: " <> show err
                Right bytes -> putStrLn $ "Success: " <> show bytes
            -- hGetLine handle >>= putStrLn
            -- _cancel ctx
            other -> putStrLn $ "A different event occurred: " <> show other
        )
    )
    [EpollIn]
    [EpollET]

  _run ctx
  pure 0
