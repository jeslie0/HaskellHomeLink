{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Minimp3
  ( MP3Dec,
    MP3DecFrameInfo,
    newMP3Dec,
    newMP3DecFrameInfo,
    getFrameBytes,
    getChannels,
    getHz,
    getLayer,
    getBitrateKPBS,
    decodeFrame,
    maxSamplesPerFrame
  )
where

import Data.Word (Word8)
import Foreign (ForeignPtr, FunPtr, Int16, Ptr, newForeignPtr)
import Foreign.C (CInt (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.ForeignPtr (withForeignPtr)

data MP3Dec_t

newtype MP3Dec = MP3Dec (ForeignPtr MP3Dec_t)

foreign import capi safe "minimp3-hs.h new_mp3dec_t" new_mp3dec_t_c :: IO (Ptr MP3Dec_t)

foreign import capi safe "minimp3-hs.h &free_mp3dec_t" free_mp3dec_t_c :: FunPtr (Ptr MP3Dec_t -> IO ())

newMP3Dec :: IO MP3Dec
newMP3Dec = do
  ptr <- new_mp3dec_t_c
  frnPtr <- newForeignPtr free_mp3dec_t_c ptr
  return $ MP3Dec frnPtr
{-# NOINLINE newMP3Dec #-}

data MP3DecFrameInfo_t

newtype MP3DecFrameInfo = MP3DecFrameInfo (ForeignPtr MP3DecFrameInfo_t)

foreign import capi safe "minimp3-hs.h new_mp3dec_frame_info_t" new_mp3dec_frame_t_c :: IO (Ptr MP3DecFrameInfo_t)

foreign import capi safe "minimp3-hs.h &free_mp3dec_frame_info_t" free_mp3dec_frame_t_c :: FunPtr (Ptr MP3DecFrameInfo_t -> IO ())

newMP3DecFrameInfo :: IO MP3DecFrameInfo
newMP3DecFrameInfo = do
  ptr <- new_mp3dec_frame_t_c
  frnPtr <- newForeignPtr free_mp3dec_frame_t_c ptr
  return $ MP3DecFrameInfo frnPtr

-- instance Show MP3DecFrameInfo where
--   {-# NOINLINE show #-}
--   show info =
--     let vals = (show . ($ info) <$> [getFrameBytes, getChannels, getHz, getLayer, getBitrateKPBS])
--      in show $ zipWith (<>) ["Frame bytes: ", "Channels: ", "Frequency Hz: ", "Layer: ", "Bitrate Kb/s: "] vals

foreign import capi safe "minimp3-hs.h get_frame_bytes_unsafe" get_frame_bytes_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

getFrameBytes :: MP3DecFrameInfo -> IO Int
getFrameBytes (MP3DecFrameInfo frnPtr) =
    withForeignPtr frnPtr $ fmap fromIntegral . get_frame_bytes_unsafe_c

foreign import capi safe "minimp3-hs.h get_channels_unsafe" get_channels_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

getChannels :: MP3DecFrameInfo -> IO Int
getChannels (MP3DecFrameInfo frnPtr) =
    withForeignPtr frnPtr $ fmap fromIntegral . get_channels_unsafe_c

foreign import capi safe "minimp3-hs.h get_hz_unsafe" get_hz_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

getHz :: MP3DecFrameInfo -> IO Int
getHz (MP3DecFrameInfo frnPtr) =
    withForeignPtr frnPtr $ fmap fromIntegral . get_hz_unsafe_c

foreign import capi safe "minimp3-hs.h get_layer_unsafe" get_layer_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

getLayer :: MP3DecFrameInfo -> IO Int
getLayer (MP3DecFrameInfo frnPtr) =
    withForeignPtr frnPtr $ fmap fromIntegral . get_layer_unsafe_c

foreign import capi safe "minimp3-hs.h get_bitrate_kbps_unsafe" get_bitrate_kbps_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

getBitrateKPBS :: MP3DecFrameInfo -> IO Int
getBitrateKPBS (MP3DecFrameInfo frnPtr) =
    withForeignPtr frnPtr $ fmap fromIntegral . get_bitrate_kbps_unsafe_c

foreign import capi safe "minimp3.h mp3dec_decode_frame"
  mp3dec_decode_frame_c ::
    Ptr MP3Dec_t ->
    ConstPtr Word8 ->
    CInt ->
    Ptr Int16 ->
    Ptr MP3DecFrameInfo_t ->
    IO CInt

decodeFrame ::
  MP3Dec ->
  -- | Input buffer
  Ptr Word8 ->
  -- | Input buffer length
  Int ->
  -- | Information which is updated
  MP3DecFrameInfo ->
  -- | Output buffer
  Ptr Int16 ->
  IO Int
decodeFrame (MP3Dec mp3DecFrnPtr) inputBuf inputLen (MP3DecFrameInfo infoFrnPtr) outputBufPtr =
  withForeignPtr mp3DecFrnPtr $
    \mp3DecPtr ->
      withForeignPtr infoFrnPtr $
        fmap fromIntegral
          . mp3dec_decode_frame_c
            mp3DecPtr
            (ConstPtr inputBuf)
            (fromIntegral inputLen)
            outputBufPtr

maxSamplesPerFrame :: Int
maxSamplesPerFrame = 2 * 1152
