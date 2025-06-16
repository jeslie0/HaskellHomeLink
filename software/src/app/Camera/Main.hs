module Camera.Main where

import Camera.VideoStream (getChunk, getInitChunk)
import Data.ByteString qualified as B
import System.IO (IOMode (ReadMode), withFile)

main :: IO ()
main = do
  withFile "/home/james/BIN" ReadMode $ \handle -> do
    init <- getInitChunk handle
    B.putStr init
    finish handle 0
 where
  finish handle !n = do
    bytes <- getChunk handle
    B.putStr bytes
    finish handle (n+1)
