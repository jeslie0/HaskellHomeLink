module HAsio.Data.ByteString where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.ByteString qualified as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Typeable (cast)
import Foreign (castPtr, mallocArray, plusPtr, free)
import Foreign.C (Errno, eAGAIN)
import HAsio.Error.ErrorStack (ErrorStack, getBaseErr)
import HAsio.Fd (IsFd)
import HAsio.Fd.Syscalls (readUnsafe)

-- We need a way to read data from a FD. Since we are generally using non-blocking IO,
-- we also need to be able to resume the extraction of data at a later
-- stage. We can use an IORef for this.

-- TODO Add more errors for when returning an error stack.
-- TODO Use resourceT to make allocation safe.
getBytes ::
  forall fd.
  IsFd fd =>
  Int
  -> ExceptT ErrorStack IO (fd -> ExceptT ErrorStack IO (Maybe B.ByteString))
getBytes len = do
  ptr <- liftIO . mallocArray $ len
  posRef <- liftIO $ newIORef (0 :: Int)
  let func fd = do
        pos <- liftIO $ readIORef posRef
        eRead <- liftIO $ readUnsafe fd (castPtr $ ptr `plusPtr` pos) (len - pos)
        case eRead of
          Right bytesRead ->
            if (bytesRead + pos) == len
              then do
              bytes <- liftIO $ B.packCStringLen (ptr, len)
              liftIO $ free ptr
              pure $ Just bytes
              else do
                liftIO (writeIORef posRef (pos + bytesRead))
                func fd
          Left errs -> do
            let baseErr = getBaseErr errs
            case (cast baseErr :: Maybe Errno) of
              -- Base err is not an errno.
              Nothing -> throwE errs
              Just err ->
                if err == eAGAIN
                  then pure Nothing
                  -- Base err is wrong errno
                  else throwE errs

  pure func
