{-# LANGUAGE TemplateHaskell #-}

module System (SystemData, cpuData, mkSystemData, inDockerContainer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Lens.Micro.TH (makeLenses)
import System.CPU (CPUData, getCPUData)
import System.Directory (doesFileExist)

data SystemData = SystemData
    { _cpuData :: CPUData
    , _inDockerContainer :: Bool
    }

$(makeLenses ''SystemData)

isInDockerContainer :: IO Bool
isInDockerContainer = do
    doesFileExist "/.dockerenv"

mkSystemData :: IO (Maybe SystemData)
mkSystemData = runMaybeT $ do
    cpuDatu' <- MaybeT getCPUData
    inDockerContainer' <- liftIO isInDockerContainer
    pure $ SystemData cpuDatu' inDockerContainer'
