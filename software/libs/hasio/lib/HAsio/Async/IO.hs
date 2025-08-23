module HAsio.Async.IO (asyncSendAll, asyncRecv, asyncAccept, RecvResult (..)) where

import Control.Exception (throwIO, try)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Typeable (cast)
import Foreign (plusPtr, withForeignPtr)
import Foreign.C (Errno, eAGAIN, eWOULDBLOCK)
import HAsio.Async.Reactor (EventHandler, ReactorCore, deregisterFd, registerFd)
import HAsio.Error.ErrorStack (ErrorStack, getBaseErr, useErrorStack, getBaseErrno)
import HAsio.Fd.Epoll (Event (EpollIn, EpollOut), Flag (EpollET))
import HAsio.Fd.Socket (
  Socket,
  SocketFlag (SocketNonBlock),
  acceptUnsafe,
  acceptUnsafe',
  recvUnsafe,
  sendUnsafe,
 )

asyncSendAll ::
  forall reactor.
  ReactorCore reactor =>
  reactor
  -> Socket
  -> B.ByteString
  -> (Either ErrorStack () -> ExceptT ErrorStack IO ())
  -> ExceptT ErrorStack IO ()
asyncSendAll reactor socket bytes callback = do
  offsetRef <- liftIO $ newIORef (0 :: Int)

  let
    trySend :: EventHandler
    trySend = do
      offset <- liftIO $ readIORef offsetRef
      let remaining = B.length bytes - offset
      if remaining <= 0
        then do
          -- TODO Check to see if it has already been closed as an error.
          -- This is not a real error here.
          deregisterFd reactor socket EpollOut
          callback (Right ())
        else do
          let (frnPtr, start, len) = B.toForeignPtr bytes

          ExceptT <$> liftIO $ withForeignPtr frnPtr $ \ptr -> do
            res <- sendUnsafe socket (ptr `plusPtr` (start + offset)) (len - offset) []
            case res of
              Left errs ->
                case getBaseErrno errs of
                  Nothing -> runExceptT . callback $ Left errs
                  Just err
                    | err == eAGAIN || err == eWOULDBLOCK -> do
                        pure $ Right ()
                    | otherwise -> do
                        eErrs <- runExceptT $ deregisterFd reactor socket EpollOut
                        case eErrs of
                          Left moreErrs -> runExceptT . callback . Left $ moreErrs <> errs
                          Right _ -> runExceptT . callback $ Left errs
              Right n -> do
                if offset + n == B.length bytes
                  then runExceptT $ do
                    deregisterFd reactor socket EpollOut
                    callback $ Right ()
                  else do
                    writeIORef offsetRef $ offset + n
                    pure $ Right ()

  registerFd reactor socket EpollOut [EpollET] trySend

recvAmount :: Int
recvAmount = 2048

data RecvResult
  = RecvData B.ByteString
  | RecvClosed

asyncRecv ::
  forall reactor.
  ReactorCore reactor =>
  reactor
  -> Socket
  -> (Either ErrorStack RecvResult -> ExceptT ErrorStack IO ())
  -> ExceptT ErrorStack IO (ExceptT ErrorStack IO ())
asyncRecv reactor socket callback = do
  let
    tryRecieve :: EventHandler
    tryRecieve = do
      drained <- runExceptT $ go []
      callback $
        drained <&> \bytes ->
          if B.null bytes
            then RecvClosed
            else RecvData bytes

    mkBytes = B.createAndTrim recvAmount $ \ptr -> do
      eBytes <- recvUnsafe socket ptr recvAmount []
      case eBytes of
        Left errs -> throwIO errs
        Right n -> pure n

    go acc = do
      eBytes <- liftIO $ try mkBytes
      case eBytes of
        Right bytes
          | B.null bytes ->
              -- Connection dropped
              pure . B.concat . reverse $ acc
          | otherwise -> go (bytes : acc)
        Left errs -> do
          case getBaseErrno errs of
            Nothing -> useErrorStack errs
            Just err
              | err == eAGAIN || err == eWOULDBLOCK -> do
                  pure . B.concat . reverse $ acc
              | otherwise -> useErrorStack errs

  registerFd reactor socket EpollIn [EpollET] tryRecieve
  pure $ deregisterFd reactor socket EpollIn

asyncAccept ::
  ReactorCore reactor =>
  reactor
  -> Socket -- listening socket
  -> (Either ErrorStack Socket -> ExceptT ErrorStack IO ())
  -> ExceptT ErrorStack IO (ExceptT ErrorStack IO ()) -- returns deregister action
asyncAccept reactor listenSock callback = do
  let
    acceptLoop :: EventHandler
    acceptLoop = do
      eConn <- liftIO tryAccept
      case eConn of
        Right conn -> do
          callback (Right conn)
          acceptLoop -- keep draining until EAGAIN
        Left errs -> case getBaseErrno errs of
          Nothing -> do
            callback (Left errs) -- fatal error
          Just err
            | err == eAGAIN || err == eWOULDBLOCK -> pure () 
            | otherwise -> do
                callback (Left errs)
    tryAccept = do
      -- your raw accept binding, returning (Left ErrorStack | Right Socket)
      acceptUnsafe listenSock [SocketNonBlock]

  registerFd reactor listenSock EpollIn [EpollET] acceptLoop

  pure $ deregisterFd reactor listenSock EpollIn
