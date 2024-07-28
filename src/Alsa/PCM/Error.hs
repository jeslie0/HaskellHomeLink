{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Alsa.PCM.Error where
import Foreign.C (CInt(..))
import Foreign.C.String (CString, peekCString)

foreign import capi unsafe "alsa/error.h snd_strerror" snd_strerror_c :: CInt -> IO CString

getErrorString :: CInt -> IO String
getErrorString n = do
  cStr <- snd_strerror_c n
  peekCString cStr
