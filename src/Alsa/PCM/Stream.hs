{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Alsa.PCM.Stream where

import Alsa.PCM.Handle
import Alsa.PCM.Params
import Data.IORef (readIORef)
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign (Ptr, Storable (..), alloca, castPtr, withForeignPtr)
import Foreign.C (CInt (..), CLong (..), CUInt, CULong (..))

data SndPCMAccess
  = MMapInterleaved
  | MMapNonInterleaved
  | MMapComplex
  | RWInterleaved
  | RWNonInterleaved
  deriving (Enum, Eq, Show)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_set_access" snd_pcm_hw_params_set_access_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> CInt -> IO CInt

setAccess :: PCMHandle -> PCMParams -> SndPCMAccess -> IO CInt
setAccess (PCMHandle handleRef) (PCMParams paramRef) access = do
  frnHandlePtr <- readIORef handleRef
  frnParamsPtr <- readIORef paramRef
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
setFormat (PCMHandle handleRef) (PCMParams paramRef) format = do
  frnHandlePtr <- readIORef handleRef
  frnParamsPtr <- readIORef paramRef
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ \paramsPtr ->
      fromIntegral <$> snd_pcm_hw_params_set_format_c handlePtr paramsPtr (fromIntegral . fromEnum $ format)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_set_channels" snd_pcm_hw_params_set_channels_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> CInt -> IO CInt

setChannels :: PCMHandle -> PCMParams -> Int -> IO CInt
setChannels (PCMHandle handleRef) (PCMParams paramRef) channels = do
  frnHandlePtr <- readIORef handleRef
  frnParamsPtr <- readIORef paramRef
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
setSampleRate (PCMHandle handleRef) (PCMParams paramRef) sampleRate = do
  frnHandlePtr <- readIORef handleRef
  frnParamsPtr <- readIORef paramRef
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ \paramsPtr ->
      alloca @CUInt $ \sampleRatePtr ->
        alloca @CInt $ \dirPtr -> do
          poke sampleRatePtr (fromIntegral sampleRate)
          err <- snd_pcm_hw_params_set_rate_near_c handlePtr paramsPtr sampleRatePtr dirPtr
          newSampleRate <- peek sampleRatePtr
          dir <- peek dirPtr
          return . SampleRate $ (fromIntegral err, fromIntegral newSampleRate, fromIntegral dir)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_set_buffer_size" snd_pcm_hw_params_set_buffer_size_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> CULong -> IO CInt

setBufferSize :: PCMHandle -> PCMParams -> Word64 -> IO Int
setBufferSize (PCMHandle handleRef) (PCMParams paramRef) size = do
  frnHandlePtr <- readIORef handleRef
  frnParamsPtr <- readIORef paramRef
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ \paramsPtr -> fromIntegral <$> snd_pcm_hw_params_set_buffer_size_c handlePtr paramsPtr (fromIntegral size)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params_set_period_size" snd_pcm_hw_params_set_period_size_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> CULong -> CInt -> IO CInt

setPeriodSize :: PCMHandle -> PCMParams -> Word64 -> Int -> IO Int
setPeriodSize (PCMHandle handleRef) (PCMParams paramRef) size dir = do
  frnHandlePtr <- readIORef handleRef
  frnParamsPtr <- readIORef paramRef
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ \paramsPtr -> fromIntegral <$> snd_pcm_hw_params_set_period_size_c handlePtr paramsPtr (fromIntegral size) (fromIntegral dir)

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_hw_params" snd_pcm_hw_params_c :: Ptr Snd_PCM_t -> Ptr Snd_PCM_HW_Params_t -> IO CInt

writeParamsToDriver :: PCMHandle -> PCMParams -> IO Int
writeParamsToDriver (PCMHandle handleRef) (PCMParams paramRef) = do
  frnHandlePtr <- readIORef handleRef
  frnParamsPtr <- readIORef paramRef
  withForeignPtr frnHandlePtr $ \handlePtr ->
    withForeignPtr frnParamsPtr $ fmap fromIntegral . snd_pcm_hw_params_c handlePtr

foreign import capi unsafe "alsa/asoundlib.h snd_pcm_writei" snd_pcm_write_i_c :: Ptr Snd_PCM_t -> Ptr () -> CULong -> IO CLong

writeBuffer :: (Storable a) => PCMHandle -> Ptr a -> Word64 -> IO Int64
writeBuffer (PCMHandle ref) bufferPtr size = do
  frnHandlePtr <- readIORef ref
  withForeignPtr frnHandlePtr $ \handlePtr ->
    fromIntegral <$> snd_pcm_write_i_c handlePtr (castPtr bufferPtr) (fromIntegral size)


foreign import capi unsafe "alsa/asoundlib.h snd_pcm_drain" snd_pcm_drain_c :: Ptr Snd_PCM_t -> IO CInt


-- | Block the thread until the audio device has finished playing it's
-- queued buffers.
drainDevice :: PCMHandle -> IO Int
drainDevice (PCMHandle ref) = do
  frnHandlePtr <- readIORef ref
  withForeignPtr frnHandlePtr $ fmap fromIntegral . snd_pcm_drain_c
