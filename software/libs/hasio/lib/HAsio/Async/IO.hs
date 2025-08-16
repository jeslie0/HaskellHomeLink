module HAsio.Async.IO (asyncSendAll) where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Typeable (cast)
import Foreign (plusPtr, withForeignPtr)
import Foreign.C (Errno, eAGAIN, eWOULDBLOCK)
import HAsio.Async.Reactor (EventHandler, ReactorCore, deregisterFd, registerFd)
import HAsio.Error.ErrorStack (ErrorStack, getBaseErr)
import HAsio.Fd.Epoll (Event (EpollOut), Flag (EpollET))
import HAsio.Fd.Socket (Socket, sendUnsafe)

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
    trySend _ = do
      offset <- liftIO $ readIORef offsetRef
      liftIO $ print $ "try send! offset = " <> show offset
      let remaining = B.length bytes - offset
      if remaining <= 0
        then do
          -- TODO Check to see if it has already been closed as an error.
          -- This is not a real error here.
          liftIO $ print "rem <= 0"
          deregisterFd reactor socket
          callback (Right ())
        else do
          let (frnPtr, start, len) = B.toForeignPtr bytes

          liftIO $ print $ "try send! remaining = " <> show remaining
          ExceptT <$> liftIO $ withForeignPtr frnPtr $ \ptr -> do
            res <- sendUnsafe socket (ptr `plusPtr` (start + offset)) (len - offset) []
            print res
            case res of
              Left errs ->
                let
                  baseErr = getBaseErr errs
                  mSysErr = cast baseErr :: Maybe Errno
                in
                  case mSysErr of
                    Nothing -> runExceptT . callback $ Left errs
                    Just err
                      | err == eAGAIN || err == eWOULDBLOCK -> do
                          liftIO $ print "eAGAIN"
                          pure $ Right ()
                      | otherwise -> do
                          eErrs <- runExceptT $ deregisterFd reactor socket
                          case eErrs of
                            Left moreErrs -> runExceptT . callback . Left $ moreErrs <> errs
                            Right _ -> runExceptT . callback $ Left errs
              Right n -> do
                if offset + n == B.length bytes
                  then 
                  runExceptT $ do
                  deregisterFd reactor socket
                  callback $ Right ()
                  else do
                    writeIORef offsetRef $ offset + n
                    pure $ Right ()

  registerFd reactor socket [EpollOut] [EpollET] trySend

-- sendUnsafe'
