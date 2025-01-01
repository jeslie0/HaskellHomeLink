{-# LANGUAGE TemplateHaskell #-}

module System (
    SystemData,
    cpuData,
    mkSystemData,
    inDockerContainer,
    systemDataToMessage,
    messageToSystemData,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ProtoLens (defMessage)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import System.CPU (CPUData, cpuDataToMessage, getCPUData, messageToCPUData)
import System.Directory (doesFileExist)

data SystemData = SystemData
    { _cpuData :: CPUData
    , _inDockerContainer :: Bool
    } deriving (Show, Eq)

$(makeLenses ''SystemData)

systemDataToMessage :: SystemData -> Proto.SystemData
systemDataToMessage (SystemData cpuData inDockerContainer) =
    defMessage
        & Proto.cpuData
        .~ cpuDataToMessage cpuData
        & Proto.inDockerContainer
        .~ inDockerContainer

messageToSystemData :: Proto.SystemData -> SystemData
messageToSystemData systemDataMessage =
    SystemData
        { _cpuData = messageToCPUData $ systemDataMessage ^. Proto.cpuData
        , _inDockerContainer = systemDataMessage ^. Proto.inDockerContainer
        }

isInDockerContainer :: IO Bool
isInDockerContainer = do
    doesFileExist "/.dockerenv"

mkSystemData :: IO (Maybe SystemData)
mkSystemData = runMaybeT $ do
    cpuDatu' <- MaybeT getCPUData
    inDockerContainer' <- liftIO isInDockerContainer
    pure $ SystemData cpuDatu' inDockerContainer'
