{-# LANGUAGE OverloadedStrings #-}

module Home.AudioStream (StreamStatus (..), startAudioStream) where

import Alsa.PCM.Handle (
    DeviceMode (PCMBlocking),
    PCMHandle,
    StreamType (Playback),
    newPCMHandle,
    openPCMHandle,
    preparePCMHandle,
 )
import Alsa.PCM.Params (allocateParams, fillParams, newPCMParams)
import Alsa.PCM.Stream (
    SndPCMAccess (RWInterleaved),
    SndPCMFormat (FormatS16LE),
    dropDevice,
    errVal,
    sampleRateVal,
    setAccess,
    setChannels,
    setFormat,
    setSampleRate,
    writeBuffer,
    writeParamsToDriver,
 )
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Foreign (Int16, Ptr, Word8, allocaArray, withForeignPtr)
import Foreign.Ptr (plusPtr)
import Minimp3 (
    MP3Dec,
    MP3DecFrameInfo,
    decodeFrame,
    getBitrateKPBS,
    getChannels,
    getFrameBytes,
    getHz,
    maxSamplesPerFrame,
    newMP3Dec,
    newMP3DecFrameInfo,
 )
import Network.HTTP.Client (BodyReader, Response)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS

data StreamStatus
    = Inactive
    | Active
    deriving (Eq, Show)

classicFMURL :: String
classicFMURL =
    "https://media-ice.musicradio.com/ClassicFMMP3"

-- unsafeTLSSettings :: HTTP.ManagerSettings
-- unsafeTLSSettings =
--     HTTPS.mkManagerSettings tlsSettings Nothing
--   where
--     tlsSettings = TLSSettingsSimple True False False

-- | Start an audio stream and pass chunks of bytes into the callback handler.
withAudioStream ::
    (Response BodyReader -> IO ())
    -- ^ Callback to use the bytes
    -> IO ()
withAudioStream withBodyRsp = do
    manager <- HTTP.newManager HTTPS.tlsManagerSettings
    let initialRequest = HTTP.parseRequest_ classicFMURL
        request = initialRequest {HTTP.method = "GET"}
    HTTP.withResponse request manager withBodyRsp

newtype SR = SR Int

newtype Channels = Channels Int

{- | Given a sample rate and a number of channels, generate a
PCMHandle to play mp3 data with.
-}
makeAudioHandle :: SR -> Channels -> IO PCMHandle
makeAudioHandle sr channels = do
    handle <- newPCMHandle
    _ <- openPCMHandle "default" Playback PCMBlocking handle
    _ <- configureDevice handle sr channels
    _ <- preparePCMHandle handle
    return handle

{- | Configure a PCMHandle with a sample rate and number of
channels for data extracted from an mp3 source.
-}
configureDevice :: PCMHandle -> SR -> Channels -> IO (Maybe Int)
configureDevice handle (SR sampleRate) (Channels channels) = do
    params <- newPCMParams
    _ <- allocateParams params
    _ <- fillParams handle params
    _ <- setAccess handle params RWInterleaved
    _ <- setFormat handle params FormatS16LE
    _ <- setChannels handle params channels
    !sr <- setSampleRate handle params $ fromIntegral sampleRate
    -- putStrLn $ "New sample rate = " <> show (sampleRateVal sr)
    if errVal sr < 0
        then putStrLn "Error: Can't set sample rate." >> return Nothing
        else do
            pcm' <- writeParamsToDriver handle params
            if pcm' < 0
                then do
                    putStrLn "Error: Can't set hardware parameters"
                    return Nothing
                else return $ Just $ sampleRateVal sr

{- | Read the input buffer as MP3 data and play as many full frames as
possible. Return the number of bytes that haven't been used.
-}
readFramesAndPlay ::
    PCMHandle
    -- ^ PCM handle
    -> MP3Dec
    -- ^ MP3 object
    -> MP3DecFrameInfo
    -- ^ Frame info object
    -> (Ptr Word8, Int)
    -- ^ Input PCM buffer
    -> Ptr Int16
    -- ^ Buffer to store data in.
    -> IO Int
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
            -- putStrLn "Skipped ID3 or Invalid data"
            return newMP3Len
        | samples == 0 && consumed == 0 = do
            -- putStrLn "Insufficient data"
            return newMP3Len
        | otherwise = do
            -- putStrLn "Impossible situation"
            return 0

-- | Compute the size of an MP3 frame from the given info.
computeFrameSize :: MP3DecFrameInfo -> IO Int
computeFrameSize info =
    (\bitrate hz -> 144 * (bitrate * 1000) `div` hz)
        <$> getBitrateKPBS info
        <*> getHz info

startAudioStream :: IO ()
startAudioStream = do
    mp3Dec <- newMP3Dec
    info <- newMP3DecFrameInfo

    allocaArray @Int16 maxSamplesPerFrame $ \pcmData ->
        withAudioStream $ \rsp -> do
            let bodyReader = HTTP.responseBody rsp
            chunk <- HTTP.brRead bodyReader
            bracket
                (makePCMHandleFromBytes mp3Dec info chunk pcmData)
                dropDevice
                $ \handle -> go bodyReader handle mp3Dec info pcmData ""
  where
    go ::
        BodyReader
        -> PCMHandle
        -> MP3Dec
        -> MP3DecFrameInfo
        -> Ptr Int16
        -> BS.ByteString
        -> IO ()
    go !bodyReader !handle !mp3Dec !info !pcmData !leftoverBytes = do
        newBytes <- HTTP.brRead bodyReader
        let !mp3Data@(BS.BS !frnPtr !mp3Len) = BS.append leftoverBytes newBytes
        !remainingBytes <- withForeignPtr frnPtr $ \mp3Ptr -> do
            !leftOverAmount <- readFramesAndPlay handle mp3Dec info (mp3Ptr, mp3Len) pcmData
            pure $ BS.takeEnd leftOverAmount mp3Data
        go bodyReader handle mp3Dec info pcmData remainingBytes

makePCMHandleFromBytes ::
    MP3Dec -> MP3DecFrameInfo -> BS.ByteString -> Ptr Int16 -> IO PCMHandle
makePCMHandleFromBytes mp3Dec info (BS.BS frnPtr mp3Len) pcmData = do
    withForeignPtr frnPtr $ \mp3Data -> do
        void $ decodeFrame mp3Dec mp3Data mp3Len info pcmData
        freq <- getHz info
        chans <- getChannels info
        makeAudioHandle (SR freq) (Channels chans)
