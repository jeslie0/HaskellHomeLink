{-# LANGUAGE OverloadedStrings #-}

module HTTP.Client (playAudio) where

import Alsa.PCM.Handle
import Alsa.PCM.Params
import Alsa.PCM.Stream
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Foreign (Int16, Ptr, Word8, allocaArray, withForeignPtr)
import Foreign.Ptr (plusPtr)
import Minimp3
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS

classicFMURL :: String
classicFMURL =
  "https://media-ice.musicradio.com/ClassicFMMP3"

-- | Start an audio stream and pass chunks of bytes into the callback handler.
withAudioStream ::
  -- | Control MVar
  MVar () ->
  -- | Callback to use the bytes
  (BS.ByteString -> IO ()) ->
  IO ()
withAudioStream continueMvar withBytes = do
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
        withBytes chunk
        withBody bodyReader

withFile :: FilePath -> (BS.ByteString -> IO ()) -> IO ()
withFile path withBytes = BS.readFile path >>= withBytes

newtype SR = SR Int

newtype Channels = Channels Int

-- | Given a sample rate and a number of channels, generate a
-- PCMHandle to play mp3 data with.
makeAudioHandle :: SR -> Channels -> IO PCMHandle
makeAudioHandle sr channels = do
  handle <- newPCMHandle
  _ <- openPCMHandle "default" Playback PCMBlocking handle
  _ <- configureDevice handle sr channels
  _ <- preparePCMHandle handle
  return handle

-- | Configure a PCMHandle with a sample rate and number of
-- channels for data extracted from an mp3 source.
configureDevice :: PCMHandle -> SR -> Channels -> IO (Maybe Int)
configureDevice handle (SR sampleRate) (Channels channels) = do
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
  -- | Buffer to store data in.
  Ptr Int16 ->
  IO Int
readFramesAndPlay handle mp3 info mp3Data pcmData =
  go mp3Data
  where
    go (mp3Ptr, !mp3Len) = do
      samples <- decodeFrame mp3 mp3Ptr mp3Len info pcmData
      consumed <- getFrameBytes info
      let newMP3Len = mp3Len - consumed
      decide mp3Ptr newMP3Len samples consumed

    decide mp3Ptr newMP3Len samples consumed
      | samples > 0 && consumed > 0 = do
          void . writeBuffer handle pcmData $ fromIntegral samples
          frameSize <- computeFrameSize info
          if frameSize > newMP3Len
            then return newMP3Len
            else go (mp3Ptr `plusPtr` consumed, newMP3Len)
      | samples == 0 && consumed > 0 = do
          putStrLn "Invalid data"
          return newMP3Len
      | samples == 0 && consumed == 0 = do
          putStrLn "Insufficient data"
          return newMP3Len
      | otherwise = do
          putStrLn "Impossible situation"
          return 0

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
  newBytes <- readChan chan
  bracket (makePCMHandleFromStream newBytes) drainDevice $ \handle -> writeChan chan newBytes >> go handle ""
  where
    go handle !leftoverBytes = do
      newBytes <- readChan chan
      let mp3Data@(BS.BS !frnPtr !mp3Len) = BS.append leftoverBytes newBytes
      withForeignPtr frnPtr $ \mp3Ptr -> do
        !leftOverAmount <- readFramesAndPlay handle mp3 info (mp3Ptr, mp3Len) pcmData
        go handle $ BS.takeEnd leftOverAmount mp3Data

    makePCMHandleFromStream (BS.BS frnPtr mp3Len) = do
      withForeignPtr frnPtr $ \mp3Data -> do
        void $ decodeFrame mp3 mp3Data mp3Len info pcmData
        freq <- getHz info
        chans <- getChannels info
        makeAudioHandle (SR freq) (Channels chans)

-- | Compute the size of an MP3 frame from the given info.
computeFrameSize :: MP3DecFrameInfo -> IO Int
computeFrameSize info =
  (\bitrate hz -> 144 * (bitrate * 1000) `div` hz) <$> getBitrateKPBS info <*> getHz info

-- | Start a stream, decode the bytes and play the resulting audio.
playAudio :: IO ()
playAudio = do
  mvar <- newEmptyMVar @()
  mp3Dec <- newMP3Dec
  chan <- newChan @BS.ByteString
  info <- newMP3DecFrameInfo
  _ <- forkIO $ withAudioStream mvar (writeChan chan)
  _ <- forkIO . allocaArray @Int16 1153 $ readChanAndPlay mp3Dec info chan
  void getLine
  putMVar mvar ()
  return ()
