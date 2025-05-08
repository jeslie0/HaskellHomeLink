{-# LANGUAGE OverloadedStrings #-}

module Camera.Camera where

import Data.ByteString qualified as B
import System.Exit (ExitCode (..))
import System.Process.ByteString (readProcessWithExitCode)

takePicture :: IO (Maybe B.ByteString)
takePicture = do
  (exit, cout, _) <- readProcessWithExitCode raspistill args ""
  case exit of
    ExitSuccess -> pure $ Just cout
    _ -> pure Nothing
 where
  raspistill = "raspistill"

  args = ["-q", "25", "-o", "-", "-w", "512", "-h", "512", "-rot", "180"]
