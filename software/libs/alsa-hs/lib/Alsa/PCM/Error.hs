{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Alsa.PCM.Error where
import Foreign.C (CInt(..), CChar)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.String (peekCString)

foreign import capi safe "alsa/error.h snd_strerror" snd_strerror_c :: CInt -> IO (ConstPtr CChar)
getErrorString :: Int -> IO String
getErrorString n = do
  ConstPtr cStr <- snd_strerror_c . fromIntegral $ n
  peekCString cStr
{-# NOINLINE getErrorString #-}
