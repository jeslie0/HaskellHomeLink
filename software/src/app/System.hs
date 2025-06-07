{-# LANGUAGE TemplateHaskell #-}

module System (
  DeviceData,
  cpuData,
  mkDeviceData,
  inDockerContainer,
  operatingSystemName,
  architecture,
  memTotalKb,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Word (Word32)
import Devices (Device)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Proto.DeviceData qualified as Proto
import Proto.DeviceData_Fields qualified as Proto
import ProtoHelper (FromMessage (fromMessage), ToMessage, toMessage)
import System.CPU (CPUData, getCPUData)
import System.Directory (doesFileExist)
import System.Info (arch, os)
import System.Memory (getTotalMemory)

data DeviceData = DeviceData
  { _cpuData :: {-# UNPACK #-} !(Maybe CPUData)
  , _inDockerContainer :: {-# UNPACK #-} !Bool
  , _operatingSystemName :: {-# UNPACK #-} !T.Text
  , _architecture :: {-# UNPACK #-} !T.Text
  , _memTotalKb :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Eq)

$(makeLenses ''DeviceData)

instance ToMessage Proto.DeviceData DeviceData where
  toMessage
    ( DeviceData
        cpuData'
        inDockerContainer'
        operatingSystemName'
        architecture'
        memTotalkB'
      ) =
      defMessage
        & Proto.maybe'cpuData
        .~ (toMessage <$> cpuData')
        & Proto.inDockerContainer
        .~ inDockerContainer'
        & Proto.operatingSystemName
        .~ operatingSystemName'
        & Proto.architecture
        .~ architecture'
        & Proto.memTotalkB
        .~ memTotalkB'

instance FromMessage Proto.DeviceData DeviceData where
  fromMessage deviceDataMsg =
    DeviceData
      { _cpuData = fromMessage <$> deviceDataMsg ^. Proto.maybe'cpuData
      , _inDockerContainer = deviceDataMsg ^. Proto.inDockerContainer
      , _operatingSystemName = deviceDataMsg ^. Proto.operatingSystemName
      , _architecture = deviceDataMsg ^. Proto.architecture
      , _memTotalKb = deviceDataMsg ^. Proto.memTotalkB
      }

instance ToMessage Proto.AllDeviceData (Map.Map Device DeviceData) where
  toMessage mp =
    defMessage
      & Proto.allDeviceData
      .~ ( Map.toList mp
            & fmap
              ( \(dev, deviceData) ->
                  defMessage
                    & Proto.device
                    .~ toMessage dev
                    & Proto.deviceData
                    .~ toMessage deviceData
              )
         )

isInDockerContainer :: IO Bool
isInDockerContainer = do
  doesFileExist "/.dockerenv"

mkDeviceData :: IO (Maybe DeviceData)
mkDeviceData = do
  mCPUData <- getCPUData
  inDockerContainer' <- liftIO isInDockerContainer
  mTotalMem <- getTotalMemory
  pure $
    ( \totalMem ->
        DeviceData
          { _cpuData = mCPUData
          , _inDockerContainer = inDockerContainer'
          , _operatingSystemName = T.pack os
          , _architecture = T.pack arch
          , _memTotalKb = totalMem
          }
    )
      <$> mTotalMem
