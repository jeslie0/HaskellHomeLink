{-# LANGUAGE OverloadedStrings #-}

module System.Memory (getTotalMemory) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Word (Word32)
import Parsing (parseKeyValuePairFile)
import Text.Read (readMaybe)

getTotalMemory :: IO (Maybe Word32)
getTotalMemory = do
  mMemMap <- parseKeyValuePairFile "/proc/meminfo" ["MemTotal"]
  pure $ do
    memMap <- mMemMap
    tValue <- memMap Map.!? "MemTotal"
    readMaybe . T.unpack . fst . T.breakOn " " $ tValue
