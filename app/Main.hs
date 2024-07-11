{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.ByteArray (ByteArrayAccess (..))
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as BI
import Data.Int (Int8)
import Foreign (pokeByteOff)
import Foreign.Ptr (Ptr, plusPtr)
import Sound.OpenAL (genObjectName, get, ($=))
import Sound.OpenAL.AL qualified as AL
import Sound.OpenAL.ALC qualified as ALC
import System.Random (getStdGen, random, setStdGen)
import System.Random.Stateful (IOGenM, newIOGenM, randomM)


frequency :: Float
frequency = 44100.0

bufferSize :: Int
bufferSize = round $ frequency / 10

bufferTimeMicroSeconds :: Int
bufferTimeMicroSeconds = round $ 1000 * frequency / fromIntegral bufferSize

-- Efficiently create a sinewave in memory.
makeSineWave :: Float -> IO B.ByteString
makeSineWave f =
  BI.create bufferSize (filler f bufferSize)

filler :: Float -> Int -> Ptr a -> IO ()
filler f n ptr = helper 0
  where
    helper !m
      | n == m = return ()
      | otherwise = do
          pokeByteOff ptr m (round @_ @Int8 $ sin (2.0 * pi * f * fromIntegral m))
          helper (m + 1)

makeBuffer :: Float -> IO AL.Buffer
makeBuffer f = do
  buffer :: AL.Buffer <- genObjectName
  updateBuffer f buffer
  return buffer

updateBuffer :: Float -> AL.Buffer -> IO ()
updateBuffer f buffer = do
  !bytes <- makeSineWave f
  withByteArray bytes $ \ptr ->
    let bufferDataSTV =
          AL.bufferData buffer

        !newMemRegion =
          AL.MemoryRegion ptr $ fromIntegral (B.length bytes)

        !newBufferData =
          AL.BufferData newMemRegion AL.Mono8 frequency
     in bufferDataSTV $= newBufferData

main :: IO ()
main = do
  -- Initialization
  maybeDevice <- ALC.openDevice Nothing -- select the "preferred device"
  case maybeDevice of
    Nothing -> return ()
    Just device -> do
      maybeContext <- ALC.createContext device streamAttributes
      ALC.currentContext $= maybeContext

      -- Generate buffers
      _errors <- AL.alErrors -- clear error code
      source :: AL.Source <- genObjectName

      !buffer1 <- makeBuffer 0.02
      !buffer2 <- makeBuffer 0.05

      AL.queueBuffers source [buffer1, buffer2]

      AL.play [source]

      gen <- getStdGen

      let loop _ 0 = print "done"
          loop gen' n = do
            threadDelay bufferTimeMicroSeconds
            state <- AL.sourceState source
            if state == AL.Stopped
              then return ()
              else do
                processed <- AL.buffersProcessed source
                if processed >= 1
                  then do
                    bufs <- AL.unqueueBuffers source processed
                    let buf = head bufs
                        (f, gen'') = random gen'
                    updateBuffer f buf
                    AL.queueBuffers source [buf]
                    setStdGen gen''
                    loop gen'' (n - 1)
                  else loop gen' n

      loop gen 25

      -- Exit
      _deviceClosed <- ALC.closeDevice device
      return ()
      where
        streamAttributes =
          [ ALC.Frequency 44100,
            ALC.MonoSources 1,
            ALC.StereoSources 0
          ]
