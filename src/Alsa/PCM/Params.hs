{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Alsa.PCM.Params where

import Alsa.PCM.Handle
import Control.Exception (mask_)
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign (ForeignPtr, FunPtr, Ptr, Storable (..), alloca, castPtr, newForeignPtr, withForeignPtr)
import Foreign.C (CInt (..), CLong (..), CUInt, CULong (..))

data Snd_PCM_HW_Params_t

foreign import capi unsafe "haskell_alsa.h snd_pcm_hw_params_t_new" snd_pcm_hw_params_t_new_c :: IO (Ptr Snd_PCM_HW_Params_t)

foreign import capi unsafe "haskell_alsa.h &snd_pcm_hw_params_t_free" snd_pcm_hw_params_t_free_c :: FunPtr (Ptr Snd_PCM_HW_Params_t -> IO ())

newtype PCMParams = PCMParams (ForeignPtr Snd_PCM_HW_Params_t)

-- Should be refactored to force manual malloc in haskell
pcmParams :: IO PCMParams
pcmParams =
  fmap PCMParams $ mask_ snd_pcm_hw_params_t_new_c >>= newForeignPtr snd_pcm_hw_params_t_free_c

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_any" snd_pcm_hw_params_any_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> IO CInt

fillParams :: PCMHandle -> PCMParams -> IO Int
fillParams (PCMHandle frnHandlePtr) (PCMParams frnParamsPtr) = do
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ fmap fromIntegral . snd_pcm_hw_params_any_c handlePtr

data SndPCMAccess
  = MMapInterleaved
  | MMapNonInterleaved
  | MMapComplex
  | RWInterleaved
  | RWNonInterleaved
  deriving (Enum, Eq, Show)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_set_access" snd_pcm_hw_params_set_access_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> CInt -> IO CInt

setAccess :: PCMHandle -> PCMParams -> SndPCMAccess -> IO CInt
setAccess (PCMHandle frnHandlePtr) (PCMParams frnParamsPtr) access = do
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ \paramsPtr ->
      fromIntegral <$> snd_pcm_hw_params_set_access_c handlePtr paramsPtr (fromIntegral . fromEnum $ access)

-- There are more, but these will be fine for now
data SndPCMFormat
  = FormatS8
  | FormatU8
  | FormatS16LE
  | FormatS16BE
  | FormatU16LE
  | FormatU16BE
  | FormatS24LE
  | FormatS24BE
  | FormatU24LE
  | FormatU24BE
  | FormatS32LE
  | FormatS32BE
  | FormatU32LE
  | FormatU32BE
  | FormatFloatLE
  | FormatFloatBR
  | FormatFloat64LE
  | FormatFloat64BE
  deriving (Enum, Eq, Show)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_set_format" snd_pcm_hw_params_set_format_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> CInt -> IO CInt

setFormat :: PCMHandle -> PCMParams -> SndPCMFormat -> IO CInt
setFormat (PCMHandle frnHandlePtr) (PCMParams frnParamsPtr) format = do
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ \paramsPtr ->
      fromIntegral <$> snd_pcm_hw_params_set_format_c handlePtr paramsPtr (fromIntegral . fromEnum $ format)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_set_channels" snd_pcm_hw_params_set_channels_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> CInt -> IO CInt

setChannels :: PCMHandle -> PCMParams -> Int -> IO CInt
setChannels (PCMHandle frnHandlePtr) (PCMParams frnParamsPtr) channels = do
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ \paramsPtr ->
      fromIntegral <$> snd_pcm_hw_params_set_channels_c handlePtr paramsPtr (fromIntegral channels)

newtype SampleRate = SampleRate (Int, Int, Int)

errVal :: SampleRate -> Int
errVal (SampleRate (n, _, _)) = n

dirVal :: SampleRate -> Int
dirVal (SampleRate (_, n, _)) = n

sampleRateVal :: SampleRate -> Int
sampleRateVal (SampleRate (_, n, _)) = n

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_set_rate_near" snd_pcm_hw_params_set_rate_near_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> Ptr CUInt -> Ptr CInt -> IO CInt

setSampleRate :: PCMHandle -> PCMParams -> Word -> IO SampleRate
setSampleRate (PCMHandle frnHandlePtr) (PCMParams frnParamsPtr) sampleRate = do
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ \paramsPtr ->
      alloca @CUInt $ \sampleRatePtr ->
        alloca @CInt $ \dirPtr -> do
          poke sampleRatePtr (fromIntegral sampleRate)
          err <- snd_pcm_hw_params_set_rate_near_c handlePtr paramsPtr sampleRatePtr dirPtr
          newSampleRate <- peek sampleRatePtr
          dir <- peek dirPtr
          return . SampleRate $ (fromIntegral err, fromIntegral newSampleRate, fromIntegral dir)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params" snd_pcm_hw_params_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> IO CInt

writeParamsToDriver :: PCMHandle -> PCMParams -> IO Int
writeParamsToDriver (PCMHandle frnHandlePtr) (PCMParams frnParamsPtr) =
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ fmap fromIntegral . snd_pcm_hw_params_c handlePtr

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_writei" snd_pcm_write_i_c :: Ptr Snd_PCM_t -> Ptr () -> CULong -> IO CLong

writeBuffer :: (Storable a) => PCMHandle -> Ptr a -> Word64 -> IO Int64
writeBuffer (PCMHandle frnHandlePtr) bufferPtr size =
  withForeignPtr frnHandlePtr $ \handlePtr ->
    fromIntegral <$> snd_pcm_write_i_c handlePtr (castPtr bufferPtr) (fromIntegral size)
