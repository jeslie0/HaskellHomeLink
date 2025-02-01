{-# LANGUAGE OverloadedStrings #-}

module Connection.Socket (
  recvNBytes,
  sendAll,
  readHeader,
) where

import Data.ByteString qualified as B
import Data.Serialize qualified as Binary
import Data.Word (Word32)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

recvNBytes :: Socket -> Int -> IO (Maybe B.ByteString)
recvNBytes sock n = do
  go n ""
 where
  go 0 acc = pure $ Just acc
  go m acc = do
    bytes <- recv sock m
    if B.length bytes == 0
      then pure Nothing
      else
        let bytesToGo = n - B.length bytes
        in go bytesToGo (acc <> bytes)

-- | We need to read 4 bytes from a socket to get the size of the
--  incoming header.
readHeader :: Socket -> IO (Maybe Word32)
readHeader sock = do
  mHdr <- recvNBytes sock 4
  case mHdr of
    Nothing -> pure Nothing
    Just hdr ->
      case Binary.runGet Binary.getWord32le hdr of
        Left str -> putStrLn ("Error: " <> str) >> pure Nothing
        Right n -> pure $ Just n
