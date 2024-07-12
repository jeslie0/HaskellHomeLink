module Other where

import Data.ByteString.Internal qualified as BI
import Data.ByteString qualified as B

import Main
import Foreign.Ptr (Ptr)
import Data.Int (Int8)
import Foreign.Storable (pokeByteOff)

-- |Efficiently create a sine wave in contiguous memory.
makeSineWave :: Float -> IO B.ByteString
makeSineWave f =
  BI.create bufferSize (filler f bufferSize)
  where
    filler :: Float -> Int -> Ptr a -> IO ()
    filler f n ptr = helper 0
      where
        helper !m
          | n == m = return ()
          | otherwise = do
              pokeByteOff ptr m (round @_ @Int8 $ 127 * sin (2.0 * pi * f * fromIntegral m))
              helper (m + 1)
