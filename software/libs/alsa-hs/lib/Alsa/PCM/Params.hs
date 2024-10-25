{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Alsa.PCM.Params (PCMParams (..), Snd_PCM_HW_Params_t, newPCMParams, allocateParams, fillParams) where

import Alsa.PCM.Handle
import Control.Exception (mask_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign (ForeignPtr, FunPtr, Ptr, Storable (..), alloca, newForeignPtr, withForeignPtr)
import Foreign.C (CInt(..))

data Snd_PCM_HW_Params_t

foreign import capi safe "haskell_alsa.h snd_pcm_hw_params_t_new" snd_pcm_hw_params_t_new_c :: IO (Ptr Snd_PCM_HW_Params_t)

foreign import capi safe "haskell_alsa.h &snd_pcm_hw_params_t_free_unused" snd_pcm_hw_params_t_free_unused_c :: FunPtr (Ptr Snd_PCM_HW_Params_t -> IO ())

newtype PCMParams = PCMParams (IORef (ForeignPtr Snd_PCM_HW_Params_t))

-- Should be refactored to force manual malloc in haskell
newPCMParams :: IO PCMParams
newPCMParams = do
  handlePtr <- snd_pcm_hw_params_t_new_c
  frnPtr <- newForeignPtr snd_pcm_hw_params_t_free_unused_c handlePtr
  ref <- newIORef frnPtr
  return $ PCMParams ref
{-# NOINLINE newPCMParams #-}

foreign import capi safe "haskell_alsa.h void_snd_pcm_hw_params_malloc" void_snd_pcm_hw_params_malloc_c :: Ptr (Ptr Snd_PCM_HW_Params_t) -> IO CInt

foreign import capi safe "alsa/asoundlib.h &snd_pcm_hw_params_free" snd_pcm_hw_params_free_c :: FunPtr (Ptr Snd_PCM_HW_Params_t -> IO ())

allocateParams :: PCMParams -> IO Int
allocateParams (PCMParams ref) = do
  frnPtr <- readIORef ref
  withForeignPtr frnPtr $ \ptr ->
    alloca $ \ptrptr -> do
      poke ptrptr ptr
      rc <- mask_ $ void_snd_pcm_hw_params_malloc_c ptrptr
      updatedParams <- peek ptrptr
      updatedFrnPtr <- newForeignPtr snd_pcm_hw_params_free_c updatedParams
      writeIORef ref updatedFrnPtr
      return . fromIntegral $ rc

{-# NOINLINE allocateParams #-}

foreign import capi safe "alsa/asoundlib.h snd_pcm_hw_params_any" snd_pcm_hw_params_any_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> IO CInt

fillParams :: PCMHandle -> PCMParams -> IO Int
fillParams (PCMHandle handleRef) (PCMParams paramRef) = do
  frnHandlePtr <- readIORef handleRef
  frnParamsPtr <- readIORef paramRef
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ fmap fromIntegral . snd_pcm_hw_params_any_c handlePtr
{-# NOINLINE fillParams #-}
