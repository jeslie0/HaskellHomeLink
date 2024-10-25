{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Alsa.PCM.Handle (PCMHandle (..), StreamType (..), DeviceMode (..), waitPCM, availablePCMFrames, newPCMHandle, openPCMHandle, preparePCMHandle, Snd_PCM_t) where

import Control.Exception (mask_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Foreign (ForeignPtr, FunPtr, Ptr, Storable (..), alloca, newForeignPtr, withForeignPtr)
import Foreign.C (CInt (..), CLong (..))
import Foreign.C.String (CString, withCString)

-- Type to represent what Alsa calls a "Sound Device". We always
-- interface with one indirectly by a pointer
data Snd_PCM_t

newtype PCMHandle = PCMHandle (IORef (ForeignPtr Snd_PCM_t))

foreign import capi safe "haskell_alsa.h snd_pcm_t_new" snd_pcm_t_new_c :: IO (Ptr Snd_PCM_t)

foreign import capi safe "haskell_alsa.h &snd_pcm_t_free_unused" snd_pcm_t_free_unused_c :: FunPtr (Ptr Snd_PCM_t -> IO ())

{-# NOINLINE newPCMHandle #-}
newPCMHandle :: IO PCMHandle
newPCMHandle = do
  handlePtr <- snd_pcm_t_new_c
  frnPtr <- newForeignPtr snd_pcm_t_free_unused_c handlePtr
  ref <- newIORef frnPtr
  return $ PCMHandle ref

foreign import capi safe "alsa/asoundlib.h &snd_pcm_close" snd_pcm_close_c :: FunPtr (Ptr Snd_PCM_t -> IO ())

data StreamType
  = Playback
  | Capture
  deriving (Eq, Show, Enum)

data DeviceMode
  = PCMBlocking
  | PCMNonBlocking
  | PCMAsync
  deriving (Eq, Show, Enum)

foreign import capi safe "haskell_alsa.h void_snd_pcm_open" void_snd_pcm_open_c :: Ptr (Ptr Snd_PCM_t) -> CString -> CInt -> CInt -> IO CInt

{-# NOINLINE openPCMHandle #-}
openPCMHandle :: String -> StreamType -> DeviceMode -> PCMHandle -> IO Int
openPCMHandle name stream mode (PCMHandle ref) = do
  frnPtr <- readIORef ref
  withForeignPtr frnPtr $ \ptr ->
    alloca $ \ptrptr -> do
      poke ptrptr ptr
      rc <- withCString name $ \cName -> mask_ $ void_snd_pcm_open_c ptrptr cName (fromIntegral . fromEnum $ stream) (fromIntegral . fromEnum $ mode)
      updatedHandlePtr <- peek ptrptr
      updatedFrnPtr <- newForeignPtr snd_pcm_close_c updatedHandlePtr
      writeIORef ref updatedFrnPtr
      return . fromIntegral $ rc

foreign import capi safe "alsa/asoundlib.h snd_pcm_prepare" snd_pcm_prepare_c :: Ptr Snd_PCM_t -> IO CInt

{-# NOINLINE preparePCMHandle #-}
preparePCMHandle :: PCMHandle -> IO Int
preparePCMHandle (PCMHandle ref) = do
  frnPtr <- readIORef ref
  withForeignPtr frnPtr $ fmap fromIntegral . snd_pcm_prepare_c

foreign import capi safe "alsa/asoundlib.h snd_pcm_avail_update" snd_pcm_avail_update_c :: Ptr Snd_PCM_t -> IO CLong

-- |Return number of frames ready to be read (capture) / written (playback)
availablePCMFrames :: PCMHandle -> IO Int64
availablePCMFrames (PCMHandle ref) = do
  frnPtr <- readIORef ref
  withForeignPtr frnPtr $ fmap fromIntegral . snd_pcm_avail_update_c
{-# NOINLINE availablePCMFrames #-}

foreign import capi safe "alsa/asoundlib.h snd_pcm_wait" snd_pcm_wait_c :: Ptr Snd_PCM_t -> CInt -> IO CInt

waitPCM :: PCMHandle -> Int -> IO Int
waitPCM (PCMHandle ref) n = do
  frnPtr <- readIORef ref
  withForeignPtr frnPtr $ \ptr -> fromIntegral <$> snd_pcm_wait_c ptr (fromIntegral n)
{-# NOINLINE waitPCM #-}
