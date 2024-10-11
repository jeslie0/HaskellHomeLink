{-# LANGUAGE OverloadedStrings #-}

module HTTP.Client where

import Alsa.PCM.Handle
import Alsa.PCM.Params
import Alsa.PCM.Stream
import Control.Concurrent
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Foreign (Int16, Ptr, Storable, Word8, allocaArray, peek, withForeignPtr)
import Foreign.Ptr (plusPtr)
import MP3
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS

classicFMURL :: String
classicFMURL = "https://media-ice.musicradio.com/ClassicFMMP3"

withAudioStream :: MVar () -> (BS.ByteString -> IO ()) -> IO ()
withAudioStream continueMvar updater = do
  manager <- HTTP.newManager HTTPS.tlsManagerSettings
  let initialRequest = HTTP.parseRequest_ classicFMURL
      request = initialRequest {HTTP.method = "GET"}
  HTTP.withResponse request manager $ \rsp -> do
    withBody $ HTTP.responseBody rsp
  where
    withBody bodyReader = do
      continue <- isEmptyMVar continueMvar
      unless continue $ do
        chunk <- HTTP.brRead bodyReader
        print "got chunk"
        updater chunk
        withBody bodyReader

sampleRate :: Int
sampleRate = 44100

channels :: Int
channels = 2

playAudio :: IO ()
playAudio = do
  handle <- newPCMHandle
  _ <- openPCMHandle "default" Playback PCMBlocking handle
  srMaybe <- configureDevice handle
  case srMaybe of
    Nothing -> return ()
    Just _sr -> do
      mvar <- newMVar ()
      _ <- preparePCMHandle handle
      mp3Dec <- newMP3Dec
      -- withAudioStream mvar $ \(BS.BS frnPointer len) -> do
      --   withForeignPtr frnPointer (convertAndPlay h 2andle mp3Dec len)
      BS.BS frnPointer len <- BS.readFile "/home/james/sample-15s.mp3"
      withForeignPtr frnPointer (convertAndPlay handle mp3Dec len)
      _ <- drainDevice handle
      _ <- getLine
      return ()
  where
    configureDevice handle = do
      params <- newPCMParams
      _ <- allocateParams params
      _ <- fillParams handle params
      _ <- setAccess handle params RWInterleaved
      _ <- setFormat handle params FormatS16LE
      _ <- setChannels handle params channels
      !sr <- setSampleRate handle params $ fromIntegral sampleRate
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

    -- Coonvert the given buffer from mp3 to PCM and play it
    convertAndPlay :: PCMHandle -> MP3Dec -> Int -> Ptr Word8 -> IO ()
    convertAndPlay handle mp3 len inputPtr = do
      allocaArray @Int16 1152 $ \outputPtr -> do
        frameInfo <- newMP3DecFrameInfo
        readFramesAndPlay handle mp3 frameInfo (inputPtr, len) (outputPtr, 1152)
        _ <- getLine
        return ()

readFrames :: MP3Dec -> MP3DecFrameInfo -> Int -> Ptr Word8 -> Int -> Ptr Int16 -> Int -> IO Int
readFrames mp3 info 0 ptr bufLen out retVal = return retVal
readFrames mp3 info !n ptr bufLen out !retVal = do
  samples <- decodeFrame mp3 ptr bufLen info out
  let consumed = fromIntegral $ getFrameBytes info
  putStrLn $ "Frame bytes: " <> show consumed
  putStrLn $ "Decoded: " <> show samples
  if samples == 0
    then do
      putStrLn "Frame bytes == 0"
      return $ retVal + samples
    else readFrames mp3 info (n - 1) (ptr `plusPtr` consumed) (bufLen - consumed) (out `plusPtr` samples) (retVal + samples)

readFramesAndPlay ::
  -- | PCM handle
  PCMHandle ->
  -- | MP3 object
  MP3Dec ->
  -- | Frame info object
  MP3DecFrameInfo ->
  -- | Input buffer
  (Ptr Word8, Int) ->
  -- | Output buffer
  (Ptr Int16, Int) ->
  IO ()
readFramesAndPlay handle mp3 info (mp3Data, mp3Len) (pcmData, pcmLength) = do
  samples <- decodeFrame mp3 mp3Data mp3Len info pcmData
  print info
  let consumed = getFrameBytes info
  if samples == 0 && consumed == 0
    then putStrLn "Insufficient data" >> print mp3Len
    else do
    _ <- writeBuffer handle pcmData $ fromIntegral samples
    readFramesAndPlay handle mp3 info (mp3Data `plusPtr` consumed, mp3Len - consumed) (pcmData, pcmLength)

printBuf :: forall a. (Show a, Storable a) => Ptr a -> Int -> IO ()
printBuf ptr 0 = return ()
printBuf ptr !n = do
  val <- peek ptr
  print val
  printBuf @a (ptr `plusPtr` 1) (n - 1)
