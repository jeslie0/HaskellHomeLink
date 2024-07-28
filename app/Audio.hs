module Audio where

import Alsa.PCM.Handle
import Alsa.PCM.Params
import Alsa.PCM.Stream
import Control.Concurrent (threadDelay)
import Data.Int (Int16)
import Data.Word (Word64)
import Foreign (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import System.Random (StdGen, getStdGen, setStdGen, uniform)

sampleRate :: Int
sampleRate = 44100

frequency :: Int
frequency = 440

duration :: Int
duration = 5

amplitude :: Int
amplitude = 32000

channels :: Int
channels = 1

bufferSize :: Word64
bufferSize = fromIntegral sampleRate

generateSineWave :: Ptr Int16 -> Word64 -> IO ()
generateSineWave ptr size = go 0
  where
    go :: Word64 -> IO ()
    go n
      | fromIntegral n == size = return ()
      | otherwise = do
          pokeElemOff @Int16 ptr (fromIntegral n) $ round @Float $ fromIntegral amplitude * sin ((2.0 * pi * fromIntegral frequency * fromIntegral n) / fromIntegral sampleRate)
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

playWhiteNoise :: IO ()
playWhiteNoise = do
  handle <- newPCMHandle
  _ <- openPCMHandle "default" Playback PCMBlocking handle
  srMaybe <- configureDevice handle
  case srMaybe of
    Nothing -> return ()
    Just sr -> do
      allocaArray sr $ \ptr -> do
        let loop = do
              _ <- writeBuffer handle ptr bufferSize
              generateWhiteNoise ptr bufferSize
              threadDelay 500000
              loop

        _ <- preparePCMHandle handle
        generateWhiteNoise ptr bufferSize
        _ <- loop
        _ <- drainDevice handle
        return ()
  where
    configureDevice handle = do
      params <- newPCMParams
      _ <- allocateParams params
      _ <- fillParams handle params
      _ <- setAccess handle params RWInterleaved
      _ <- setFormat handle params FormatS16LE
      _ <- setChannels handle params channels
      !sr <- setSampleRate handle params 44100
      putStrLn $ "New sample rate = " <> show (sampleRateVal sr)
      if errVal sr < 0
        then putStrLn "Error: Can't set sample rate." >> return Nothing
        else do
          pcm' <- writeParamsToDriver handle params
          if pcm' < 0
            then do
              putStrLn "Error: Can't set hardware parameters"
              return Nothing
            else return $ Just $ sampleRateVal sr
