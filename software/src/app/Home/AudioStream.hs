{-# LANGUAGE OverloadedStrings #-}

module Home.AudioStream (
    StreamStatus (..),
    startAudioStream,
    StationId
) where

import Alsa.PCM.Handle (
    DeviceMode (PCMBlocking),
    PCMHandle,
    StreamType (Playback),
    newPCMHandle,
    openPCMHandle,
    preparePCMHandle,
    recoverPCM,
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
import Control.Exception (Exception (..), bracket, catch)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.Text qualified as T
import Foreign (Int16, Ptr, Word8, allocaArray, withForeignPtr)
import Foreign.C (Errno (..))
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
import Network.HTTP.Types.Status (ok200)
import Proto.Messages qualified as Proto
import ProtoHelper (FromMessage (fromMessage), ToMessage (toMessage))
import System.Timeout (timeout)
import Data.Word (Word32)

type StationId = Word32

data StreamStatus = Off | Initiated | Playing
    deriving (Eq)

instance FromMessage Proto.STREAM_STATUS StreamStatus where
    fromMessage Proto.OFF = Off
    fromMessage Proto.INITIATED = Initiated
    fromMessage Proto.PLAYING = Playing
    fromMessage _ = Off

instance ToMessage Proto.STREAM_STATUS StreamStatus where
    toMessage Off = Proto.OFF
    toMessage Initiated = Proto.INITIATED
    toMessage Playing = Proto.PLAYING

-- unsafeTLSSettings :: HTTP.ManagerSettings
-- unsafeTLSSettings =
--     HTTPS.mkManagerSettings tlsSettings Nothing
--   where
--     tlsSettings = TLSSettingsSimple True False False

-- | Start an audio stream and pass chunks of bytes into the callback handler.
withAudioStream ::
    T.Text
    -> (Response BodyReader -> IO ())
    -- ^ Callback to use the bytes
    -> IO ()
withAudioStream url withBodyRsp = do
    manager <- HTTP.newManager HTTPS.tlsManagerSettings
    let initialRequest = HTTP.parseRequest_ $ T.unpack url
        request = initialRequest {HTTP.method = "GET"}
    HTTP.withResponse request manager withBodyRsp `catch` \(e :: HTTP.HttpException) -> do
        putStrLn $ "An HTTP Exception occurred: " <> displayException e

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
possible. Return the number of bytes that haven't been used, or an
error code if one has been given.
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
    -> IO (Either Errno Int)
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
            samplesWritten <- writeBuffer handle pcmData $ fromIntegral samples
            if samplesWritten < 0
                then
                    pure $ Left (Errno . fromIntegral $ -samplesWritten)
                else do
                    frameSize <- computeFrameSize info
                    if frameSize > newMP3Len
                        then pure $ Right newMP3Len
                        else go (mp3Ptr `plusPtr` consumed, newMP3Len)
        | samples == 0 && consumed > 0 = do
            -- putStrLn "Skipped ID3 or Invalid data"
            pure $ Right newMP3Len
        | samples == 0 && consumed == 0 = do
            -- putStrLn "Insufficient data"
            pure $ Right newMP3Len
        | otherwise = do
            -- putStrLn "Impossible situation"
            pure $ Right 0

-- | Compute the size of an MP3 frame from the given info.
computeFrameSize :: MP3DecFrameInfo -> IO Int
computeFrameSize info =
    (\bitrate hz -> 144 * (bitrate * 1000) `div` hz)
        <$> getBitrateKPBS info
        <*> getHz info

type URL = T.Text

startAudioStream :: URL -> (StreamStatus -> IO ()) -> IO ()
startAudioStream url updateStreamStatus = do
    updateStreamStatus Initiated

    mp3Dec <- newMP3Dec
    info <- newMP3DecFrameInfo

    allocaArray @Int16 maxSamplesPerFrame $ \pcmData ->
        withAudioStream url $ \rsp -> do
            if HTTP.responseStatus rsp /= ok200
                then
                    putStrLn "Bad status from response"
                else do
                    let bodyReader = HTTP.responseBody rsp
                    chunk <- HTTP.brRead bodyReader
                    bracket
                        (makePCMHandleFromBytes mp3Dec info chunk pcmData)
                        dropDevice
                        $ \handle -> do
                            updateStreamStatus Playing
                            go bodyReader handle mp3Dec info pcmData ""

    updateStreamStatus Off
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
        mNewBytes <- timeout (1000000 * 5) $ HTTP.brRead bodyReader
        case mNewBytes of
            Nothing -> pure ()
            Just newBytes -> do
                if BS.length newBytes == 0
                    then putStrLn "Stream over"
                    else do
                        let !mp3Data@(BS.BS !frnPtr !mp3Len) = BS.append leftoverBytes newBytes
                        !eRemainingBytes <- withForeignPtr frnPtr $ \mp3Ptr -> do
                            eLeftOverAmount <- readFramesAndPlay handle mp3Dec info (mp3Ptr, mp3Len) pcmData
                            case eLeftOverAmount of
                                Left err -> do
                                    putStrLn "Underflow occurred"
                                    void $ recoverPCM handle err False
                                    pure . Right $ mp3Data
                                Right leftOverAmount -> pure . Right $ BS.takeEnd leftOverAmount mp3Data
                        case eRemainingBytes of
                            Left _ -> pure ()
                            Right remainingBytes ->
                                go bodyReader handle mp3Dec info pcmData remainingBytes

makePCMHandleFromBytes ::
    MP3Dec -> MP3DecFrameInfo -> BS.ByteString -> Ptr Int16 -> IO PCMHandle
makePCMHandleFromBytes mp3Dec info (BS.BS frnPtr mp3Len) pcmData = do
    withForeignPtr frnPtr $ \mp3Data -> do
        void $ decodeFrame mp3Dec mp3Data mp3Len info pcmData
        freq <- getHz info
        chans <- getChannels info
        makeAudioHandle (SR freq) (Channels chans)
