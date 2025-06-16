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
) where

import Control.Exception
import Control.Exception (throw)
import Data.ByteString qualified as B
import Data.Serialize (getWord32be, runGet)
import Lens.Micro.TH (makeLenses)
import System.IO (Handle, hSetBinaryMode)
import System.Process (
  CreateProcess (std_in, std_out),
  ProcessHandle,
  StdStream (CreatePipe, UseHandle),
  cleanupProcess,
  createProcess_,
  proc,
 )

raspividProc :: CreateProcess
raspividProc =
  (proc "raspivid" args) {std_out = CreatePipe}
 where
  args = duration <> useStdOut <> inlineHeaders <> quality <> fps <> dunno <> vflip

  duration = ["-t", "0"]

  useStdOut = ["-o", "-"]

  inlineHeaders = ["-ih"]

  quality = ["-pf", "high"]

  fps = ["-fps", "30"]

  dunno = ["-g", "60"]

  vflip = ["-vflip"]

ffmpegProc :: Handle -> CreateProcess
ffmpegProc input =
  (proc "ffmpeg" args) {std_in = UseHandle input}
 where
  args =
    [ "-f"
    , "h264"
    , "-i"
    , "pipe:0"
    , "-c"
    , "copy"
    , "-movflags"
    , "+frag_keyframe+empty_moov+default_base_moof"
    , "-f"
    , "mp4"
    , "pipe:1"
    , "-loglevel"
    , "error"
    , "-nostats"
    ]

data VideoStreamResource = VideoStreamResource
  { _raspividProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  , _ffmpegProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  }

$(makeLenses ''VideoStreamResource)

cleanupVideoStreamResource :: VideoStreamResource -> IO ()
cleanupVideoStreamResource (VideoStreamResource {_raspividProcess, _ffmpegProcess}) = do
  cleanupProcess _ffmpegProcess
  cleanupProcess _raspividProcess

createVideoStreamResource :: IO VideoStreamResource
createVideoStreamResource = do
  _raspividProcess@(_, Just _raspividOut, _, _) <-
    createProcess_ "Oops! Raspivid failed." raspividProc
  hSetBinaryMode _raspividOut True
  _ffmpegProcess@(_, Just _ffmpegOut, _, _) <-
    createProcess_ "Oops! FFMPEG failed." $ ffmpegProc _raspividOut
  hSetBinaryMode _ffmpegOut True
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
