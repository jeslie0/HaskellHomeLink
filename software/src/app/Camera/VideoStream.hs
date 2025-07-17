{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera.VideoStream (
  VideoStreamResource,
  raspividProcess,
  ffmpegProcess,
  cleanupVideoStreamResource,
  createVideoStreamResource,
  getInitChunk,
  getChunk,
  getStreamHandle,
  VideoMessage(..),
  startVideoStream,
) where

import Control.Exception (bracket)
import Data.ByteString qualified as B
import Data.Serialize (getWord32be, runGet)
import Data.Text qualified as T
import Lens.Micro ((^.), _2)
import Lens.Micro.TH (makeLenses)
import System.IO (
  BufferMode (NoBuffering),
  Handle,
  hSetBinaryMode,
  hSetBuffering,
 )
import System.Process (
  CreateProcess (std_in, std_out),
  ProcessHandle,
  StdStream (CreatePipe, UseHandle),
  cleanupProcess,
  createProcess_,
  proc,
 )

raspividProc :: [T.Text] -> CreateProcess
raspividProc args =
  (proc "raspivid" $ T.unpack <$> args) {std_out = CreatePipe}

-- where

-- args = duration <> useStdOut <> inlineHeaders <> quality <> fps <> dunno <> vflip

-- duration = ["-t", "0"]

-- useStdOut = ["-o", "-"]

-- inlineHeaders = ["-ih"]

-- quality = ["-pf", "high"]

-- fps = ["-fps", "24"]

-- dunno = ["-g", "60"]

-- vflip = ["--vflip"]

ffmpegProc :: Handle -> [T.Text] -> CreateProcess
ffmpegProc input args =
  (proc "ffmpeg" $ T.unpack <$> args) {std_in = UseHandle input, std_out = CreatePipe}
 -- where
  -- args =
  --   concat
  --     [ ["-f", "h264"]
  --     , ["-i", "pipe:0"]
  --     , ["-c:v", "copy"]
  --     , ["-movflags", "+frag_keyframe+empty_moov+default_base_moof"]
  --     -- , ["-min_frag_duration", show @Int $ 5 * floor @Double (10 ** 5)]
  --     , ["-f", "mp4"]
  --     , ["pipe:1"]
  --     , ["-loglevel", "error"]
  --     , ["-nostats"]
  --     ]

data VideoStreamResource = VideoStreamResource
  { _raspividProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  , _ffmpegProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  }

$(makeLenses ''VideoStreamResource)

getStreamHandle :: VideoStreamResource -> Maybe Handle
getStreamHandle res = res ^. ffmpegProcess . _2

cleanupVideoStreamResource :: VideoStreamResource -> IO ()
cleanupVideoStreamResource (VideoStreamResource {_raspividProcess, _ffmpegProcess}) = do
  cleanupProcess _ffmpegProcess
  cleanupProcess _raspividProcess

createVideoStreamResource :: [T.Text] -> [T.Text] -> IO VideoStreamResource
createVideoStreamResource raspiArgs ffmpegOpts= do
  _raspividProcess@(_, Just _raspividOut, _, _) <-
    createProcess_ "Oops! Raspivid failed." $ raspividProc raspiArgs
  hSetBinaryMode _raspividOut True
  hSetBuffering _raspividOut NoBuffering
  _ffmpegProcess@(_, Just _ffmpegOut, _, _) <-
    createProcess_ "Oops! FFMPEG failed." $ ffmpegProc _raspividOut ffmpegOpts
  hSetBinaryMode _ffmpegOut True
  hSetBuffering _ffmpegOut NoBuffering
  pure $
    VideoStreamResource {_raspividProcess, _ffmpegProcess}

data InitBoxType = Moov | Ftyp

getInitChunk :: Handle -> IO B.ByteString
getInitChunk handle = do
  helper ""
 where
  helper acc = do
    !sizeBytes <- B.hGet handle 4
    !boxTypeBytes <- B.hGet handle 4
    let
      Right len = runGet getWord32be sizeBytes
      boxType
        | boxTypeBytes == "moov" = Moov
        | boxTypeBytes == "ftyp" = Ftyp
        | otherwise = undefined
    !boxBytes <- B.hGet handle (fromIntegral $ len - 8)
    case boxType of
      Moov ->
        pure $ acc <> sizeBytes <> boxTypeBytes <> boxBytes
      Ftyp ->
        helper $ acc <> sizeBytes <> boxTypeBytes <> boxBytes

data BoxType = Moof | Mdat

getChunkInfo :: Handle -> IO (BoxType, B.ByteString)
getChunkInfo handle = do
  !sizeBytes <- B.hGet handle 4
  !boxTypeBytes <- B.hGet handle 4
  let
    Right len = runGet getWord32be sizeBytes
    boxType
      | boxTypeBytes == "moof" = Moof
      | boxTypeBytes == "mdat" = Mdat
      | boxTypeBytes == "moov" = error "NOT MOOV"
      | boxTypeBytes == "ftyp" = error "NOT FTYP"
      | otherwise = error "COULD NOT GET BOX TYPE"
  !boxBytes <- B.hGet handle (fromIntegral $ len - 8)
  pure (boxType, sizeBytes <> boxTypeBytes <> boxBytes)

getChunk :: Handle -> IO B.ByteString
getChunk handle = do
  helper ""
 where
  helper acc = do
    (boxtype, boxBytes) <- getChunkInfo handle
    case boxtype of
      Moof -> do
        helper $ acc <> boxBytes
      Mdat -> pure $ acc <> boxBytes

data VideoMessage = InitMsg B.ByteString | ChunkMsg B.ByteString

startVideoStream :: (VideoMessage -> IO ()) -> [T.Text] -> [T.Text] -> IO ()
startVideoStream addMsg raspiArgs ffmpegOpts =
  bracket (createVideoStreamResource raspiArgs ffmpegOpts) cleanupVideoStreamResource $ \res -> do
  let Just handle = getStreamHandle res
  sendInitChunk handle
  sendVidData handle
 where
  sendInitChunk handle = do
    bytes <- getInitChunk handle
    addMsg $ InitMsg bytes

  sendVidData handle = do
    bytes <- getChunk handle
    addMsg $ ChunkMsg bytes
    sendVidData handle
