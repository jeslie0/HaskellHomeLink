{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module System.Memory (
  getTotalMemory,
  MemoryInformation,
  systemTimeNs,
  memUsedkB,
  getMemoryInformation,
  device,
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
import Devices (Device)
import Lens.Micro ((.~), (^.))
import Lens.Micro.TH (makeLenses)
import Parsing (parseKeyValuePairFile)
import Proto.DeviceData qualified as Proto
import Proto.DeviceData_Fields qualified as Proto
import ProtoHelper (FromMessage (..), ToMessage (..))
import Text.Read (readMaybe)

data MemoryInformation = MemoryInformation
  { _systemTimeNs :: {-# UNPACK #-} !Word64
  , _memUsedkB :: {-# UNPACK #-} !Word32
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

data DeviceMemoryInformation = DeviceMemoryInformation
  { _device :: {-# UNPACK #-} !Device
  , _timeMem :: {-# UNPACK #-} !(V.Vector MemoryInformation)
  }

instance ToMessage Proto.DeviceMemoryInformation DeviceMemoryInformation where
  toMessage (DeviceMemoryInformation island' memInfos') =
    defMessage
      & Proto.device .~ toMessage island'
      & Proto.timeMem .~ V.toList (toMessage <$> memInfos')

instance FromMessage Proto.DeviceMemoryInformation DeviceMemoryInformation where
  fromMessage msg =
    DeviceMemoryInformation
      { _device = fromMessage $ msg ^. Proto.device
      , _timeMem = V.fromList $ fromMessage <$> msg ^. Proto.timeMem
      }

$(makeLenses ''DeviceMemoryInformation)

instance
  ToMessage
    Proto.AllDeviceMemoryData
    (Map.Map Device (V.Vector MemoryInformation))
  where
  toMessage memMap =
    defMessage
      & Proto.allDeviceMemoryData
        .~ ( Map.toList memMap <> Map.toList memMap
              <&> toMessage . uncurry DeviceMemoryInformation
           )

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
  pure $
    MemoryInformation
      (fromIntegral sec * 1000 + (fromIntegral nanosec `div` 1000000))
      <$> mem
