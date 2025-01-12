{-# LANGUAGE TemplateHaskell #-}

module System (
  SystemData,
  cpuData,
  mkSystemData,
  inDockerContainer,
  operatingSystemName,
  architecture,
  memTotalKb
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (FromMessage (fromMessage), ToMessage, toMessage)
import System.CPU (CPUData, getCPUData)
import System.Directory (doesFileExist)
import System.Info (arch, os)
import Data.Word (Word32)
import System.Memory (getTotalMemory)
import qualified Data.Map.Strict as Map
import Islands (Island)

data SystemData = SystemData
  { _cpuData :: CPUData
  , _inDockerContainer :: Bool
  , _operatingSystemName :: T.Text
  , _architecture :: T.Text
  , _memTotalKb :: Word32
  }
  deriving (Show, Eq)

$(makeLenses ''SystemData)

instance ToMessage Proto.SystemData SystemData where
  toMessage (SystemData cpuData' inDockerContainer' operatingSystemName' architecture' memTotalkB') =
    defMessage
      & Proto.cpuData
      .~ toMessage cpuData'
      & Proto.inDockerContainer
      .~ inDockerContainer'
      & Proto.operatingSystemName
      .~ operatingSystemName'
      & Proto.architecture
      .~ architecture'
      & Proto.memTotalkB
      .~ memTotalkB'

instance FromMessage Proto.SystemData SystemData where
  fromMessage systemDataMessage =
    SystemData
      { _cpuData = fromMessage $ systemDataMessage ^. Proto.cpuData
      , _inDockerContainer = systemDataMessage ^. Proto.inDockerContainer
      , _operatingSystemName = systemDataMessage ^. Proto.operatingSystemName
      , _architecture = systemDataMessage ^. Proto.architecture
      , _memTotalKb = systemDataMessage ^. Proto.memTotalkB
      }

instance ToMessage Proto.IslandsSystemData (Map.Map Island SystemData) where
  toMessage mp =
    defMessage
      & Proto.allSystemData
      .~ ( Map.toList mp
            & fmap
              ( \(island, sysData) ->
                  defMessage
                    & Proto.island
                    .~ toMessage island
                    & Proto.systemData
                    .~ toMessage sysData
              )
         )


isInDockerContainer :: IO Bool
isInDockerContainer = do
  doesFileExist "/.dockerenv"

mkSystemData :: IO (Maybe SystemData)
mkSystemData = runMaybeT $ do
  cpuData' <- MaybeT getCPUData
  inDockerContainer' <- liftIO isInDockerContainer
  totalMem <- MaybeT getTotalMemory
  pure $
    SystemData
      { _cpuData = cpuData'
      , _inDockerContainer = inDockerContainer'
      , _operatingSystemName = T.pack os
      , _architecture = T.pack arch
      , _memTotalKb = totalMem
      }
