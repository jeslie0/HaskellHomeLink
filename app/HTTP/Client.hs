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

-- | Start an audio stream, writing the audio payload into the
-- channel.
withAudioStream :: MVar () -> Chan BS.ByteString -> IO ()
withAudioStream continueMvar chan = do
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
        writeChan chan chunk
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
      chan <- newChan
      threadId <- forkIO $ withAudioStream mvar chan
      allocaArray @Int16 1152 $ \pcmPtr -> do
        info <- newMP3DecFrameInfo
        _ <- readChanAndPlay handle mp3Dec info chan (pcmPtr, 1152)
        return ()
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

-- | Read the input buffer as MP3 data and play as many full frames as
-- possible. Return the number of bytes that haven't been used.
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
  IO Int
readFramesAndPlay handle mp3 info (mp3Data, mp3Len) (pcmData, pcmLength) = do
  samples <- decodeFrame mp3 mp3Data mp3Len info pcmData
  let consumed = getFrameBytes info
      newMP3Len = mp3Len - consumed
  if samples == 0 && consumed == 0
    then return mp3Len
    else do
      _ <- writeBuffer handle pcmData $ fromIntegral samples
      if computeFrameSize info > newMP3Len
        then return newMP3Len
        else readFramesAndPlay handle mp3 info (mp3Data `plusPtr` consumed, newMP3Len) (pcmData, pcmLength)

readChanAndPlay ::
  -- | PCM handle
  PCMHandle ->
  -- | MP3 object
  MP3Dec ->
  -- | Frame info object
  MP3DecFrameInfo ->
  -- | Input Channel
  Chan BS.ByteString ->
  -- | Output buffer
  (Ptr Int16, Int) ->
  IO ()
readChanAndPlay handle mp3 info chan (pcmData, pcmLength) = do
  go ""
  where
    go :: BS.ByteString -> IO ()
    go leftoverBytes = do
      newBytes <- readChan chan
      let mp3Data@(BS.BS frnPtr mp3Len) = BS.append leftoverBytes newBytes
      withForeignPtr frnPtr $ \mp3Ptr -> do
        leftOverAmount <- readFramesAndPlay handle mp3 info (mp3Ptr, mp3Len) (pcmData, pcmLength)
        go $ BS.takeEnd leftOverAmount mp3Data

computeFrameSize :: MP3DecFrameInfo -> Int
computeFrameSize info =
  144 * (getBitrateKPBS info * 1000) `div` getHz info
