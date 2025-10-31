{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HAsio.Async.TLS (
  TLSConnection,
  mkTLSConnection,
  HAsio.Async.TLS.setBlocking,
  HAsio.Async.TLS.setNonBlocking,
  handShakeTLSConnection,
  registerTLSConnection,
  sendData,
  close,
  closeSession,
) where

import Control.Exception (Exception (..), catch, throwIO, try)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Data.IORef (
  IORef,
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Foreign (plusPtr, withForeignPtr)
import HAsio.Async.IO
import HAsio.Async.Reactor (Reactor)
import HAsio.Error.Error (Error (..))
import HAsio.Error.ErrorCategory (ErrorCategory (..))
import HAsio.Error.ErrorStack (ErrorStack, makeErrorStack)
import HAsio.Fd (Fd, IsFd (..), setBlocking, setNonBlocking)
import HAsio.Fd.Socket (Socket, recv_, send_)
import Network.TLS qualified as TLS
import HAsio.Fd.Syscalls (closeUnsafe_, closeUnsafe')

data TLSErrorCategory

instance ErrorCategory TLSErrorCategory where
  getErrorCategoryName = "tls"

newtype TLSError = TLSError TLS.TLSException

instance Error TLSError where
  type ECat TLSError = TLSErrorCategory

  getErrorMessage (TLSError ex) = T.pack $ displayException ex

data WouldBlock = WouldBlock deriving (Show, Typeable)

instance Exception WouldBlock

data TLSConnection = TLSConnection
  { fd :: Fd
  , ctx :: TLS.Context
  , recvFunc :: IORef (Int -> IO B.ByteString)
  , sendFunc :: IORef (B.ByteString -> IO ())
  , flushFunc :: IORef (IO ())
  , -- TODO Change these to ringed buffers
    processedBuf :: IORef B.ByteString
  , inBuf :: IORef B.ByteString
  , outBuf :: IORef B.ByteString
  }

-- | Can throw an error stack
blockingRecvImpl :: Socket -> Int -> IO B.ByteString
blockingRecvImpl sock n =
  B.createAndTrim n $ \ptr -> do
    recv_ sock ptr n []

blockingSendImpl :: Socket -> B.ByteString -> IO ()
blockingSendImpl sock bytes = do
  let (frnPtr, start, len) = B.toForeignPtr bytes
  withForeignPtr frnPtr $ \ptr -> do
    go (ptr `plusPtr` start) len
 where
  go _ 0 = pure ()
  go ptr len = do
    sent <- send_ sock ptr len []
    go (ptr `plusPtr` sent) (len - sent)

blockingFlushImpl :: IO ()
blockingFlushImpl = pure ()

nonblockingFlushImpl :: IORef B.ByteString -> Reactor -> Socket -> IO ()
nonblockingFlushImpl bytesRef reactor sock = do
  bytes <- readIORef bytesRef
  res <- runExceptT $ asyncSendAll reactor sock bytes $ \case
    Left errs -> liftIO $ print errs
    Right _ -> pure ()
  case res of
    Left errs -> print errs
    Right _ -> pure ()

-- | Expects Fd to be in blocking mode. It will be changed to
-- nonblocking after the handshake. If this function returns a
-- TLSConnection, it takes partial ownership of the given Fd (see
-- closeSession and close).
mkTLSConnection ::
  TLS.TLSParams params => params -> Fd -> ExceptT ErrorStack IO TLSConnection
mkTLSConnection params fd = liftIO $ do
  inBuf <- newIORef B.empty
  outBuf <- newIORef B.empty
  recvFunc <- newIORef $ blockingRecvImpl (fromFd fd)
  sendFunc <- newIORef $ blockingSendImpl (fromFd fd)
  flushFunc <- newIORef blockingFlushImpl
  processedBuf <- newIORef B.empty
  let backend =
        TLS.Backend
          { TLS.backendFlush = pure ()
          , TLS.backendClose = closeUnsafe_ fd
          , TLS.backendSend = \bytes -> readIORef sendFunc >>= ($ bytes)
          , TLS.backendRecv = \n -> readIORef recvFunc >>= ($ n)
          }
  ctx <- TLS.contextNew backend params
  pure $
    TLSConnection
      { fd
      , ctx
      , recvFunc
      , sendFunc
      , flushFunc
      , processedBuf
      , inBuf
      , outBuf
      }

setBlocking :: TLSConnection -> ExceptT ErrorStack IO ()
setBlocking conn = liftIO $ do
  HAsio.Fd.setBlocking (fd conn)
  writeIORef (recvFunc conn) $ blockingRecvImpl (fromFd $ fd conn)
  writeIORef (sendFunc conn) $ blockingSendImpl (fromFd $ fd conn)
  writeIORef (flushFunc conn) blockingFlushImpl

setNonBlocking :: Reactor -> TLSConnection -> ExceptT ErrorStack IO ()
setNonBlocking reactor conn = liftIO $ do
  HAsio.Fd.setNonBlocking (fd conn)
  writeIORef (recvFunc conn) $ backendRecvImpl (processedBuf conn) (inBuf conn)
  writeIORef (sendFunc conn) $
    backendSendImpl reactor (fromFd $ fd conn) (outBuf conn)

backendRecvImpl ::
  IORef B.ByteString -> IORef B.ByteString -> Int -> IO B.ByteString
backendRecvImpl processedBuf inBuf n = do
  bs <- readIORef inBuf
  processed <- readIORef processedBuf
  if B.length bs < n
    then do
      writeIORef inBuf (processed <> bs)
      writeIORef processedBuf B.empty
      throwIO WouldBlock
    else
      let (init', rest) = B.splitAt n bs
      in do
          writeIORef inBuf rest
          writeIORef processedBuf (processed <> init')
          pure init'

backendSendImpl ::
  Reactor -> Socket -> IORef B.ByteString -> B.ByteString -> IO ()
backendSendImpl reactor sock outBuf bytes = do
  oldBytes <- readIORef outBuf
  let newMsg = oldBytes <> bytes
  writeIORef outBuf newMsg
  nonblockingFlushImpl outBuf reactor sock

-- | Need to add error detection to all of the functions here. Should
-- return an ErrorStack on failure.
handShakeTLSConnection ::
  TLSConnection
  -> ExceptT ErrorStack IO ()
handShakeTLSConnection conn = do
  ExceptT $
    catch (Right <$> TLS.handshake @IO (ctx conn)) $
      \(e :: TLS.TLSException) -> runExceptT . makeErrorStack $ TLSError e

registerTLSConnection ::
  Reactor
  -> TLSConnection
  -> (B.ByteString -> ExceptT ErrorStack IO ())
  -> ExceptT ErrorStack IO (ExceptT ErrorStack IO ())
registerTLSConnection reactor tlsConnection withBytes = do
  asyncRecv reactor (fromFd $ fd tlsConnection) $ \dereg -> \case
    RecvClosed -> dereg
    RecvData bytes -> do
      liftIO $ do
        storedBytes <- readIORef (inBuf tlsConnection)
        writeIORef (inBuf tlsConnection) (storedBytes <> bytes)
      processTLSData
     where
      processTLSDataUnsafe :: ExceptT ErrorStack IO ()
      processTLSDataUnsafe = do
        eRes <- liftIO . try $ recvData tlsConnection
        case eRes of
          Left WouldBlock -> pure ()
          Right bs
            | B.null bs -> pure ()
            | otherwise -> do
                withBytes bs
                processTLSDataUnsafe

      processTLSData :: ExceptT ErrorStack IO ()
      processTLSData = do
        ExceptT <$> catch (runExceptT processTLSDataUnsafe) $
          \(e :: TLS.TLSException) -> runExceptT . makeErrorStack $ TLSError e

-- | Can throw WouldBlock, TLSException, IOException, although
-- IOException shouldn't happen due to our Backend implementation.
recvData :: TLSConnection -> IO B.ByteString
recvData conn = do
  bytes <- TLS.recvData (ctx conn)
  liftIO $ writeIORef (processedBuf conn) B.empty
  pure bytes

-- | Can throw WouldBlock, TLSException
sendData :: TLSConnection -> B.ByteString -> ExceptT ErrorStack IO ()
sendData conn bytes = do
  ExceptT <$> catch (Right <$> TLS.sendData @IO (ctx conn) (B.fromStrict bytes)) $
    \(e :: TLS.TLSException) -> runExceptT . makeErrorStack $ TLSError e

-- | Close the TLS Connection but keep the underlying FD alive.
closeSession :: TLSConnection -> ExceptT ErrorStack IO ()
closeSession = do
  liftIO . TLS.bye . ctx

-- | Close the TLS connection then close the underlying FD.
close :: TLSConnection -> ExceptT ErrorStack IO ()
close conn = do
  liftIO . TLS.bye . ctx $ conn
  closeUnsafe' . fd $ conn
