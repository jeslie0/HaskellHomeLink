{-# LANGUAGE TemplateHaskell #-}

module System (
    SystemData,
    cpuData,
    mkSystemData,
    inDockerContainer,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ProtoLens (defMessage)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (FromMessage (fromMessage), ToMessage, toMessage)
import System.CPU (CPUData, getCPUData)
import System.Directory (doesFileExist)

data SystemData = SystemData
    { _cpuData :: CPUData
    , _inDockerContainer :: Bool
    }
    deriving (Show, Eq)

$(makeLenses ''SystemData)

instance ToMessage Proto.SystemData SystemData where
    toMessage (SystemData cpuData' inDockerContainer') =
        defMessage
            & Proto.cpuData
            .~ toMessage cpuData'
            & Proto.inDockerContainer
            .~ inDockerContainer'

instance FromMessage Proto.SystemData SystemData where
    fromMessage systemDataMessage =
        SystemData
            { _cpuData = fromMessage $ systemDataMessage ^. Proto.cpuData
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
