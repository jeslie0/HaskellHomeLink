module Audio where

import Alsa.PCM.Handle
import Alsa.PCM.Params
import Data.Word (Word64)
import Foreign.Ptr (Ptr)
import Data.Int (Int16)
import Foreign.Storable (Storable(..))
import Foreign (allocaArray)
import System.Random (getStdGen, StdGen, uniform, setStdGen)
import Control.Concurrent (threadDelay)

sampleRate = 44100
frequency = 440
duration = 5
amplitude = 32000
channels = 1
bufferSize :: Word64
bufferSize = sampleRate

generateSineWave :: Ptr Int16 -> Word64 -> IO ()
generateSineWave ptr size = go 0
  where
    go :: Word64 -> IO ()
    go n
      | fromIntegral n == size = return ()
      | otherwise = do
          pokeElemOff @Int16 ptr (fromIntegral n) $ round $ amplitude * sin ((2.0 * pi * frequency * fromIntegral (n)) / fromIntegral sampleRate)
          go (n + 1)

generateWhiteNoise :: Ptr Int16 -> Word64 -> IO ()
generateWhiteNoise ptr size = do
  gen <- getStdGen
  go gen 0
  where
    go :: StdGen -> Word64 -> IO ()
    go gen n
      | fromIntegral n == size = setStdGen gen
      | otherwise = do
          let (val, gen') = uniform gen
          pokeElemOff @Int16 ptr (fromIntegral n) val
          go gen' (n + 1)

test :: IO ()
test = do
  handle <- pcmHandle "default" Playback 0
  params <- pcmParams
  _ <- fillParams handle params
  _ <- setAccess handle params RWInterleaved
  _ <- setFormat handle params FormatS16LE
  _ <- setChannels handle params channels
  !sr <- setSampleRate handle params 44100
  print $ "New sample rate = " <> show (sampleRateVal sr)
  if errVal sr < 0
    then putStrLn "Error: Can't set sample rate."
    else do
      pcm' <- writeParamsToDriver handle params
      if pcm' < 0
        then putStrLn "Error: Can't set hardware parameters"
        else do
          allocaArray (fromIntegral $ sampleRateVal sr) $ \ptr -> do
            let
                loop = do
                  _ <- writeBuffer handle ptr bufferSize
                  generateWhiteNoise ptr bufferSize
                  threadDelay 500000
                  loop

            generateWhiteNoise ptr bufferSize
            loop
