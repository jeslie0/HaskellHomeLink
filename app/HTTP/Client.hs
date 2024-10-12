{-# LANGUAGE OverloadedStrings #-}

module HTTP.Client where

import Alsa.PCM.Handle
import Alsa.PCM.Params
import Alsa.PCM.Stream
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Foreign (Int16, Ptr, Word8, allocaArray, withForeignPtr)
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
      when continue $ do
        chunk <- HTTP.brRead bodyReader
        writeChan chan chunk
        withBody bodyReader

playAudio :: IO ()
playAudio = do
  mvar <- newEmptyMVar @()
  mp3Dec <- newMP3Dec
  chan <- newChan
  info <- newMP3DecFrameInfo
  _ <- forkIO $ withAudioStream mvar chan
  _ <- forkIO . allocaArray @Int16 1153 $ readChanAndPlay mp3Dec info chan
  _ <- getLine
  putMVar mvar ()
  return ()

newtype SR = SR Int

newtype Channels = Channels Int


-- | Given a sample rate and a number of channels, generate a
-- PCMHandle to play mp3 data with.
makeAudioHandle :: SR -> Channels -> IO PCMHandle
makeAudioHandle (SR sampleRate) (Channels channels) = do
  handle <- newPCMHandle
  _ <- openPCMHandle "default" Playback PCMBlocking handle
  _ <- configureDevice handle
  _ <- preparePCMHandle handle
  return handle
  where
    -- | Configure a PCMHandle with a sample rate and number of
    -- channels for data extracted from an mp3 source.
    configureDevice :: PCMHandle -> IO (Maybe Int)
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
  -- | Input PCM buffer
  (Ptr Word8, Int) ->
  Ptr Int16 ->
  IO Int
readFramesAndPlay handle mp3 info mp3Data pcmData =
  go mp3Data
  where
    go (mp3Ptr, mp3Len) = do
      samples <- decodeFrame mp3 mp3Ptr mp3Len info pcmData
      let consumed = getFrameBytes info
          newMP3Len = mp3Len - consumed
      if samples == 0 && consumed == 0
        then return mp3Len
        else do
          unless (samples == 0 && consumed > 0) $ void $ do
            void . writeBuffer handle pcmData $ fromIntegral samples
          if computeFrameSize info > newMP3Len
            then return newMP3Len
            else go (mp3Ptr `plusPtr` consumed, newMP3Len)

-- | Get bytes from the channel, decode them to PCM and then play the audio.
readChanAndPlay ::
  -- | MP3 object
  MP3Dec ->
  -- | Frame info object
  MP3DecFrameInfo ->
  -- | Input Channel
  Chan BS.ByteString ->
  -- | Buffer to store PCM data in
  Ptr Int16 ->
  IO ()
readChanAndPlay mp3 info chan pcmData = do
  bracket makePCMHandleFromStream drainDevice $ \handle -> go handle ""
  where
    go handle leftoverBytes = do
      newBytes <- readChan chan
      let mp3Data@(BS.BS frnPtr mp3Len) = BS.append leftoverBytes newBytes
      withForeignPtr frnPtr $ \mp3Ptr -> do
        leftOverAmount <- readFramesAndPlay handle mp3 info (mp3Ptr, mp3Len) pcmData
        go handle $ BS.takeEnd leftOverAmount mp3Data

    makePCMHandleFromStream = do
      BS.BS frnPtr mp3Len <- readChan chan
      withForeignPtr frnPtr $ \mp3Data -> do
        void $ decodeFrame mp3 mp3Data mp3Len info pcmData
        makeAudioHandle (SR $ getHz info) (Channels $ getChannels info)

computeFrameSize :: MP3DecFrameInfo -> Int
computeFrameSize info =
  144 * (getBitrateKPBS info * 1000) `div` getHz info
