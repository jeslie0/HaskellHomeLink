{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Test where

import Control.IOContext (
  SomeToFd (..),
  ToFd (..),
  mkIOContext,
  _cancel,
  _register,
  _run,
 )
import Data.ByteString qualified as B
import Data.IORef (newIORef, readIORef)
import Foreign (Ptr, allocaArray, castPtr, Word8)
import Foreign.C (CInt(..), CSize(..), Errno (Errno), getErrno, errnoToIOError)
import GHC.IO.Handle.FD (fdToHandle)
import System.Epoll (Event (..), Flag (..))
import System.IO (
  BufferMode (NoBuffering),
  hGetContents,
  hGetLine,
  hSetBuffering,
  stdin,
 )
import System.Posix (
  CSsize(..),
  Fd,
  FdOption (NonBlockingRead),
  setFdMode,
  setFdOption,
 )
import System.Posix.ByteString (fdRead, fdReadBuf)
import Control.Exception (IOException)
import System.Posix (Fd(..))

foreign import capi unsafe "unistd.h read"
  c_read_unsafe :: CInt -> Ptr () -> CSize -> IO CSsize

c_read :: Fd -> Ptr Word8 -> CSize -> IO (Either IOException Int)
c_read (Fd fd) ptr len = do
  n <- c_read_unsafe fd (castPtr ptr) len
  if n < 0 then do
    err <- getErrno
    pure . Left $ errnoToIOError "c_read" err Nothing Nothing
  else pure . Right . fromIntegral $ n
  

type Acc = Maybe (B.ByteString, Int)

trackingCallback :: Fd -> IO ()
trackingCallback fd = do
  allocaArray 10 $ \ptr -> do
    go ptr
 where
  go ptr = do
    bytesRead <- c_read fd ptr 1024
    case bytesRead of
      Left err -> print err
      Right bytesRead ->
       putStrLn ("Bytes read : " <> show bytesRead) >> go ptr

-- bufferRef <- newIORef Nothing
-- pure $ \evs -> do
--   readIORef bufferRef
--   _

main :: IO Int
main = do
  Right ctx <- mkIOContext
  fd <- toFd stdin -- this detaches the Handle from the runtime!
  setFdOption fd NonBlockingRead True
  _register
    ctx
    ( SomeToFd fd
    , \_ -> trackingCallback fd
    )
    [EpollIn]
    [EpollET]

  _run ctx
  pure 0
