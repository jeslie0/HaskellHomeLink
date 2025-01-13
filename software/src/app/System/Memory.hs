{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module System.Memory (
  getTotalMemory,
  MemoryInformation,
  systemTimeNs,
  memUsedkB,
  getMemoryInformation,
  island,
  timeMem,
) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Islands (Island)
import Lens.Micro ((.~), (^.))
import Lens.Micro.TH (makeLenses)
import Parsing (parseKeyValuePairFile)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (FromMessage (..), ToMessage (..))
import Text.Read (readMaybe)

data MemoryInformation = MemoryInformation
  { _systemTimeNs :: !Word64
  , _memUsedkB :: !Word32
  }
  deriving (Eq, Show)

$(makeLenses ''MemoryInformation)

instance ToMessage Proto.MemoryInformation MemoryInformation where
  toMessage (MemoryInformation systemTimeMs' memUsedkB') =
    defMessage
      & Proto.pair .~ [fromIntegral systemTimeMs', fromIntegral memUsedkB']

instance FromMessage Proto.MemoryInformation MemoryInformation where
  fromMessage msg =
    let (time : mem : _) = msg ^. Proto.pair
    in MemoryInformation
        { _systemTimeNs = round time
        , _memUsedkB = round mem
        }

data IslandMemoryInformation = IslandMemoryInformation
  { _island :: Island
  , _timeMem :: V.Vector MemoryInformation
  }

instance ToMessage Proto.IslandMemoryInformation IslandMemoryInformation where
  toMessage (IslandMemoryInformation island' memInfos') =
    defMessage
      & Proto.island .~ toMessage island'
      & Proto.timeMem .~ V.toList (toMessage <$> memInfos')

instance FromMessage Proto.IslandMemoryInformation IslandMemoryInformation where
  fromMessage msg =
    IslandMemoryInformation
      { _island = fromMessage $ msg ^. Proto.island
      , _timeMem = V.fromList $ fromMessage <$> msg ^. Proto.timeMem
      }

$(makeLenses ''IslandMemoryInformation)

instance
  ToMessage
    Proto.AllIslandMemoryData
    (Map.Map Island (V.Vector MemoryInformation))
  where
  toMessage memMap =
    defMessage
      & Proto.allIslandMemoryData
        .~ (Map.toList memMap <&> toMessage . uncurry IslandMemoryInformation)

getTotalMemory :: IO (Maybe Word32)
getTotalMemory = do
  mMemMap <- parseKeyValuePairFile "/proc/meminfo" ["MemTotal"]
  pure $ do
    memMap <- mMemMap
    tValue <- memMap Map.!? "MemTotal"
    readMaybe . T.unpack . fst . T.breakOn " " $ tValue

getUsedMemory :: IO (Maybe Word32)
getUsedMemory = do
  mMemMap <- parseKeyValuePairFile "/proc/meminfo" ["MemTotal", "MemAvailable"]
  pure $ do
    memMap <- mMemMap
    memTotalTxt <- memMap Map.!? "MemTotal"
    memTotal <- readMaybe . T.unpack . fst . T.breakOn " " $ memTotalTxt
    memFreeTxt <- memMap Map.!? "MemAvailable"
    memFree <- readMaybe . T.unpack . fst . T.breakOn " " $ memFreeTxt
    pure $ memTotal - memFree

getMemoryInformation :: IO (Maybe MemoryInformation)
getMemoryInformation = do
  mem <- getUsedMemory
  MkSystemTime sec nanosec <- getSystemTime
  pure $ MemoryInformation (fromIntegral sec * 1000 + (fromIntegral nanosec `div` 1000000)) <$> mem
