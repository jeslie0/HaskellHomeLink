{-# LANGUAGE OverloadedStrings #-}

module Socket (recvNBytes, sendBytes, readHeader) where

import Data.ByteString qualified as B
import Data.Serialize qualified as Binary
import Data.Word (Word32)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, send)

recvNBytes :: Socket -> Int -> IO B.ByteString
recvNBytes sock n = do
  go n ""
  where
    go 0 acc = pure acc
    go m acc = do
      bytes <- recv sock m
      let bytesToGo = n - B.length bytes
      go bytesToGo (acc <> bytes)

sendBytes :: Socket -> B.ByteString -> IO ()
sendBytes sock bytes = do
  go bytes
  where
    go "" = pure ()
    go remainingBytes = do
      sent <- send sock remainingBytes
      go $ B.drop sent remainingBytes

-- | We need to read 4 bytes from a socket to get the size of the
--  incoming header.
readHeader :: Socket -> IO Word32
readHeader sock = do
  hdr <- recvNBytes sock 4
  case Binary.runGet Binary.getWord32le hdr of
    Left str -> putStrLn str >> pure 0
    Right n -> pure n
