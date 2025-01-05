{-# LANGUAGE TemplateHaskell #-}

module System (
  SystemData,
  cpuData,
  mkSystemData,
  inDockerContainer,
  operatingSystemName,
  architecture,
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

data SystemData = SystemData
  { _cpuData :: CPUData
  , _inDockerContainer :: Bool
  , _operatingSystemName :: T.Text
  , _architecture :: T.Text
  }
  deriving (Show, Eq)

$(makeLenses ''SystemData)

instance ToMessage Proto.SystemData SystemData where
  toMessage (SystemData cpuData' inDockerContainer' operatingSystemName' architecture') =
    defMessage
      & Proto.cpuData
      .~ toMessage cpuData'
      & Proto.inDockerContainer
      .~ inDockerContainer'
      & Proto.operatingSystemName
      .~ operatingSystemName'
      & Proto.architecture
      .~ architecture'

instance FromMessage Proto.SystemData SystemData where
  fromMessage systemDataMessage =
    SystemData
      { _cpuData = fromMessage $ systemDataMessage ^. Proto.cpuData
      , _inDockerContainer = systemDataMessage ^. Proto.inDockerContainer
      , _operatingSystemName = systemDataMessage ^. Proto.operatingSystemName
      , _architecture = systemDataMessage ^. Proto.architecture
      }

isInDockerContainer :: IO Bool
isInDockerContainer = do
  doesFileExist "/.dockerenv"

mkSystemData :: IO (Maybe SystemData)
mkSystemData = runMaybeT $ do
  cpuData' <- MaybeT getCPUData
  inDockerContainer' <- liftIO isInDockerContainer
  pure $
    SystemData
      { _cpuData = cpuData'
      , _inDockerContainer = inDockerContainer'
      , _operatingSystemName = T.pack os
      , _architecture = T.pack arch
      }
