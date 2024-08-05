{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Alsa.PCM.Error where
import Foreign.C (CInt(..), CChar)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.String (peekCString)

foreign import capi unsafe "alsa/error.h snd_strerror" snd_strerror_c :: CInt -> IO (ConstPtr CChar)

getErrorString :: CInt -> IO String
getErrorString n = do
  ConstPtr cStr <- snd_strerror_c n
  peekCString cStr
