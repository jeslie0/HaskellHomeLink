{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HAsio.Async.TLS where

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Data.Foldable (for_)
import Data.IORef (
  IORef,
  modifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Typeable (Typeable)
import Foreign (plusPtr, withForeignPtr)
import Foreign.C (Errno, eWOULDBLOCK)
import Foreign.C.Error (eAGAIN)
import GHC.Exception (Exception)
import HAsio.Async.IO
import HAsio.Async.Reactor (Reactor)
import HAsio.Error.ErrorStack (ErrorStack)
import HAsio.Fd (Fd, IsFd (..), setBlocking, setNonBlocking)
import HAsio.Fd.Socket (Socket, recvUnsafe_, recv_, sendUnsafe_, send_)
import Network.TLS qualified as TLS

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
blockingRecvImpl sock n = do
  print $ "BLOCKING RECV CALLED with n = " <> show n
  B.createAndTrim n $ \ptr -> do
    recv_ sock ptr n []

blockingSendImpl :: Socket -> B.ByteString -> IO ()
blockingSendImpl sock bytes = do
  print "BLOCKING SEND CALLED"
  let (frnPtr, start, len) = B.toForeignPtr bytes
  withForeignPtr frnPtr $ \ptr -> do
    go (ptr `plusPtr` start) len
 where
  go _ 0 = print "go with n = 0" >> pure ()
  go ptr len = do
    print $ "GO WITH n = " <> show len
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
-- nonblocking after the handshake.
mkTLSConnection :: TLS.TLSParams params => params -> Fd -> IO TLSConnection
mkTLSConnection params fd = do
  inBuf <- newIORef B.empty
  outBuf <- newIORef B.empty
  recvFunc <- newIORef $ blockingRecvImpl (fromFd fd)
  sendFunc <- newIORef $ blockingSendImpl (fromFd fd)
  flushFunc <- newIORef blockingFlushImpl
  processedBuf <- newIORef B.empty
  let backend =
        TLS.Backend
          { backendFlush = pure ()
          , backendClose = pure ()
          , backendSend = \bytes -> readIORef sendFunc >>= ($ bytes)
          , backendRecv = \n -> readIORef recvFunc >>= ($ n)
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

setBlocking :: TLSConnection -> IO ()
setBlocking conn = do
  HAsio.Fd.setBlocking (fd conn)
  writeIORef (recvFunc conn) $ blockingRecvImpl (fromFd $ fd conn)
  writeIORef (sendFunc conn) $ blockingSendImpl (fromFd $ fd conn)
  writeIORef (flushFunc conn) blockingFlushImpl

setNonBlocking :: Reactor -> TLSConnection -> IO ()
setNonBlocking reactor conn = do
  HAsio.Fd.setNonBlocking (fd conn)
  writeIORef (recvFunc conn) $ backendRecvImpl (processedBuf conn) (inBuf conn)
  writeIORef (sendFunc conn) $
    backendSendImpl reactor (fromFd $ fd conn) (outBuf conn)

-- writeIORef (flushFunc conn) $
--   nonblockingFlushImpl (outBuf conn) reactor (fromFd $ fd conn)

backendRecvImpl ::
  IORef B.ByteString -> IORef B.ByteString -> Int -> IO B.ByteString
backendRecvImpl processedBuf inBuf n = do
  print $ "backend recv impl - wanting " <> show n <> " bytes"
  bs <- readIORef inBuf
  processed <- readIORef processedBuf
  print $ "STORED BYTES SIZE: " <> show (B.length bs)
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
  print "backend send impl"
  oldBytes <- readIORef outBuf
  let newMsg = oldBytes <> bytes
  writeIORef outBuf newMsg
  nonblockingFlushImpl outBuf reactor sock

data FlushResult
  = Flushed
  | WouldBlockPending
  | FatalSend Errno

-- attemptFlush ::
--   TLSConnection -> (Fd -> B.ByteString -> IO (Either Errno Int)) -> IO FlushResult
-- attemptFlush TLSConnection {..} sendRaw = do
--   out <- readIORef outBuf
--   if B.null out
--     then do
--       writeIORef wantsWrite True
--       pure Flushed
--     else do
--       let go bs = do
--             if B.null bs
--               then pure Flushed
--               else do
--                 err <- sendRaw fd bs
--                 case err of
--                   Right n -> go (B.drop n bs)
--                   Left errno ->
--                     if errno == eAGAIN || errno == eWOULDBLOCK
--                       then pure WouldBlockPending
--                       else pure $ FatalSend errno
--       res <- go out
--       case res of
--         Flushed -> do
--           writeIORef outBuf B.empty
--           writeIORef wantsWrite False
--           pure Flushed
--         WouldBlockPending -> pure WouldBlockPending
--         FatalSend errno ->
--           pure $ FatalSend errno

-- | Need to add error detection to all of the functions here. Should
-- return an ErrorStack on failure.
handShakeTLSConnection ::
  TLSConnection
  -> IO ()
handShakeTLSConnection = do
  TLS.handshake . ctx

registerTLSConnection ::
  Reactor
  -> TLSConnection
  -> (B.ByteString -> ExceptT ErrorStack IO ())
  -> ExceptT ErrorStack IO (ExceptT ErrorStack IO ())
registerTLSConnection reactor tlsConnection withBytes = do
  asyncRecv reactor (fromFd $ fd tlsConnection) $ \dereg -> \case
    RecvClosed -> dereg
    RecvData bytes -> do
      liftIO $ print $ "Read: " <> show (B.length bytes)
      liftIO $ do
        storedBytes <- readIORef (inBuf tlsConnection)
        writeIORef (inBuf tlsConnection) (storedBytes <> bytes)
        processTLSData
     where
      -- (Just <$> TLS.recvData (ctx tlsConnection))
      --   `catch` (\WouldBlock -> pure Nothing)
      -- case mBytes of
      --   Nothing -> liftIO $ print "WOULD BLOCK"
      --   Just bytes' -> withBytes bytes'

      processTLSData = do
        let loop = do
              eRes <- try $ recvData tlsConnection
              case eRes of
                Left WouldBlock -> print "WOULD BLACK"
                Right bs
                  | B.null bs -> pure ()
                  | otherwise -> do
                      _ <- runExceptT (withBytes bs)
                      loop -- <--- call recvData again immediately
        loop

-- for_ mBytes withBytes

recvData :: MonadIO m => TLSConnection -> m B.ByteString
recvData conn = do
  bytes <- TLS.recvData (ctx conn)
  liftIO $ writeIORef (processedBuf conn) B.empty
  pure bytes
