module HIO.Data.ByteString where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Typeable (cast)
import Foreign (castPtr, mallocArray, plusPtr)
import Foreign.C (Errno, eAGAIN)
import HIO.Error.ErrorStack (ErrorStack, getBaseErr)
import HIO.Fd (IsFd)
import HIO.Fd.Syscalls (readUnsafe)

-- We need a way to read data from a FD. Since we are generally using non-blocking IO,
-- we also need to be able to resume the extraction of data at a later
-- stage. We can use an IORef for this.

-- TODO Add more errors for when returning an error stack
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
              then liftIO $ Just <$> B.packCStringLen (ptr, len)
              else do
                liftIO (writeIORef posRef (pos + bytesRead))
                func fd
          Left errs -> do
            let baseErr = getBaseErr errs
            case (cast baseErr :: Maybe Errno) of
              -- Base err is not an errno.
              Nothing -> ExceptT . pure $ Left errs
              Just err ->
                if err == eAGAIN
                  then pure Nothing
                  -- Base err is wrong errno
                  else ExceptT . pure $ Left errs

  pure func
