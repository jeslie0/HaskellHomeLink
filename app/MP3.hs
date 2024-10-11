{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module MP3
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
  )
where

import Data.Word (Word8)
import Foreign (ForeignPtr, FunPtr, Int16, Ptr, newForeignPtr)
import Foreign.C (CInt (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.ForeignPtr (withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)

data MP3Dec_t

newtype MP3Dec = MP3Dec (ForeignPtr MP3Dec_t)

foreign import capi unsafe "my_mini.h new_mp3dec_t" new_mp3dec_t_c :: IO (Ptr MP3Dec_t)

foreign import capi unsafe "my_mini.h &free_mp3dec_t" free_mp3dec_t_c :: FunPtr (Ptr MP3Dec_t -> IO ())

newMP3Dec :: IO MP3Dec
newMP3Dec = do
  ptr <- new_mp3dec_t_c
  frnPtr <- newForeignPtr free_mp3dec_t_c ptr
  return $ MP3Dec frnPtr

data MP3DecFrameInfo_t

newtype MP3DecFrameInfo = MP3DecFrameInfo (ForeignPtr MP3DecFrameInfo_t)

foreign import capi unsafe "my_mini.h new_mp3dec_frame_info_t" new_mp3dec_frame_t_c :: IO (Ptr MP3DecFrameInfo_t)

foreign import capi unsafe "my_mini.h &free_mp3dec_frame_info_t" free_mp3dec_frame_t_c :: FunPtr (Ptr MP3DecFrameInfo_t -> IO ())

newMP3DecFrameInfo :: IO MP3DecFrameInfo
newMP3DecFrameInfo = do
  ptr <- new_mp3dec_frame_t_c
  frnPtr <- newForeignPtr free_mp3dec_frame_t_c ptr
  return $ MP3DecFrameInfo frnPtr


instance Show MP3DecFrameInfo where
  {-# NOINLINE show #-}
  show info =
    let vals = (show . ($ info) <$> [getFrameBytes, getChannels, getHz, getLayer, getBitrateKPBS])
    in
     show $ zipWith (<>) ["Frame bytes: ", "Channels: ", "Frequency Hz: ", "Layer: ", "Bitrate Kb/s: "] vals



foreign import capi unsafe "my_mini.h get_frame_bytes_unsafe" get_frame_bytes_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

{-# NOINLINE getFrameBytes #-}
getFrameBytes :: MP3DecFrameInfo -> Int
getFrameBytes (MP3DecFrameInfo frnPtr) =
  unsafePerformIO $ do
    withForeignPtr frnPtr $ fmap fromIntegral . get_frame_bytes_unsafe_c

foreign import capi unsafe "my_mini.h get_channels_unsafe" get_channels_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

{-# NOINLINE getChannels #-}
getChannels :: MP3DecFrameInfo -> Int
getChannels (MP3DecFrameInfo frnPtr) =
  unsafePerformIO $ do
    withForeignPtr frnPtr $ fmap fromIntegral . get_channels_unsafe_c

foreign import capi unsafe "my_mini.h get_hz_unsafe" get_hz_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

{-# NOINLINE getHz #-}
getHz :: MP3DecFrameInfo -> Int
getHz (MP3DecFrameInfo frnPtr) =
  unsafePerformIO $ do
    withForeignPtr frnPtr $ fmap fromIntegral . get_hz_unsafe_c

foreign import capi unsafe "my_mini.h get_layer_unsafe" get_layer_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

{-# NOINLINE getLayer #-}
getLayer :: MP3DecFrameInfo -> Int
getLayer (MP3DecFrameInfo frnPtr) =
  unsafePerformIO $ do
    withForeignPtr frnPtr $ fmap fromIntegral . get_layer_unsafe_c

foreign import capi unsafe "my_mini.h get_bitrate_kbps_unsafe" get_bitrate_kbps_unsafe_c :: Ptr MP3DecFrameInfo_t -> IO CInt

{-# NOINLINE getBitrateKPBS #-}
getBitrateKPBS :: MP3DecFrameInfo -> Int
getBitrateKPBS (MP3DecFrameInfo frnPtr) =
  unsafePerformIO $ do
    withForeignPtr frnPtr $ fmap fromIntegral . get_bitrate_kbps_unsafe_c

foreign import capi unsafe "minimp3.h mp3dec_decode_frame"
  mp3dec_decode_frame_c ::
    Ptr MP3Dec_t ->
    ConstPtr Word8 ->
    CInt ->
    Ptr Int16 ->
    Ptr MP3DecFrameInfo_t ->
    IO CInt

{-# NOINLINE decodeFrame #-}
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
