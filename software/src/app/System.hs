{-# LANGUAGE TemplateHaskell #-}

module System (
  SystemData,
  cpuData,
  mkSystemData,
  inDockerContainer,
  operatingSystemName,
  architecture,
  memTotalKb,
  island
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Word (Word32)
import Islands (Island)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (FromMessage (fromMessage), ToMessage, toMessage)
import System.CPU (CPUData, getCPUData)
import System.Directory (doesFileExist)
import System.Info (arch, os)
import System.Memory (getTotalMemory)

data SystemData = SystemData
  { _cpuData :: {-# UNPACK #-} !(Maybe CPUData)
  , _inDockerContainer :: {-# UNPACK #-} !Bool
  , _operatingSystemName :: {-# UNPACK #-} !T.Text
  , _architecture :: {-# UNPACK #-} !T.Text
  , _memTotalKb :: {-# UNPACK #-} !(Maybe Word32)
  , _island :: {-# UNPACK #-} !Island
  }
  deriving (Show, Eq)

$(makeLenses ''SystemData)

instance ToMessage Proto.SystemData SystemData where
  toMessage ( SystemData
                cpuData'
                inDockerContainer'
                operatingSystemName'
                architecture'
                memTotalkB'
                island'
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
      & Proto.maybe'memTotalkB
      .~ memTotalkB'
      & Proto.island
      .~ toMessage island'

instance FromMessage Proto.SystemData SystemData where
  fromMessage systemDataMessage =
    SystemData
      { _cpuData = fromMessage <$> systemDataMessage ^. Proto.maybe'cpuData
      , _inDockerContainer = systemDataMessage ^. Proto.inDockerContainer
      , _operatingSystemName = systemDataMessage ^. Proto.operatingSystemName
      , _architecture = systemDataMessage ^. Proto.architecture
      , _memTotalKb = systemDataMessage ^. Proto.maybe'memTotalkB
      , _island = fromMessage $ systemDataMessage ^. Proto.island
      }

instance ToMessage Proto.IslandsSystemData (Map.Map Island SystemData) where
  toMessage mp =
    defMessage
      & Proto.allSystemData
      .~ ( Map.toList mp
            & fmap
              ( \(island', sysData) ->
                  defMessage
                    & Proto.island
                    .~ toMessage island'
                    & Proto.systemData
                    .~ toMessage sysData
              )
         )

isInDockerContainer :: IO Bool
isInDockerContainer = do
  doesFileExist "/.dockerenv"

mkSystemData :: Island -> IO SystemData
mkSystemData island' = do
  mCPUData <- getCPUData
  inDockerContainer' <- liftIO isInDockerContainer
  totalMem <- getTotalMemory
  pure $
    SystemData
      { _cpuData = mCPUData
      , _inDockerContainer = inDockerContainer'
      , _operatingSystemName = T.pack os
      , _architecture = T.pack arch
      , _memTotalKb = totalMem
      , _island = island'
      }
