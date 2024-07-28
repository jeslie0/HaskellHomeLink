{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Alsa.PCM.Handle where

import Control.Exception (mask_)
import Foreign (ForeignPtr, FunPtr, Ptr, Storable (..), alloca, newForeignPtr, withForeignPtr)
import Foreign.C (CInt (..))
import Foreign.C.String (CString, withCString)

data Snd_PCM_t

foreign import capi unsafe "haskell_alsa.h snd_pcm_t_new" snd_pcm_t_new_c :: CString -> CInt -> CInt -> IO (Ptr Snd_PCM_t)

foreign import capi unsafe "haskell_alsa.h &snd_pcm_t_free" snd_pcm_t_free_c :: FunPtr (Ptr Snd_PCM_t -> IO ())

foreign import capi unsafe "haskell_alsa.h open_pcm_for_stream" open_handle_for_steam_c :: Ptr (Ptr Snd_PCM_t) -> CString -> IO CInt

newtype PCMHandle = PCMHandle (ForeignPtr Snd_PCM_t)

data StreamType =
  Playback | Capture deriving (Eq, Show, Enum)

pcmHandle :: String -> StreamType -> Int -> IO PCMHandle
pcmHandle name stream mode =
  withCString name $ \cName ->
  fmap PCMHandle $ mask_ (snd_pcm_t_new_c cName (fromIntegral . fromEnum $ stream) (fromIntegral mode)) >>= newForeignPtr snd_pcm_t_free_c

openForStream :: PCMHandle -> String -> IO (Int, PCMHandle)
openForStream (PCMHandle frnPtr) name =
  withForeignPtr frnPtr $ \ptr ->
    withCString name $ \cString ->
      alloca $ \ptrptr -> do
        poke ptrptr ptr
        retVal <- open_handle_for_steam_c ptrptr cString
        newPtr <- peek ptrptr
        newFrnPtr <- newForeignPtr snd_pcm_t_free_c newPtr
        return (fromIntegral retVal, PCMHandle newFrnPtr)
