{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.CPU (
  CPUData (..),
  CPUDataError (..),
  modelName,
  getCPUData,
) where

import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Parsing (parseKeyValuePairFile)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (FromMessage (..), ToMessage (..))

newtype CPUData = CPUData
  { _modelName :: T.Text
  }
  deriving (Show, Eq)

$(makeLenses ''CPUData)

data CPUDataError = NoModelName

instance ToMessage Proto.CPUData CPUData where
  toMessage cpuData =
    defMessage
      & Proto.modelName
      .~ (cpuData ^. modelName)

instance FromMessage Proto.CPUData CPUData where
  fromMessage msg = do
    CPUData
      { _modelName = msg ^. Proto.modelName
      }

getCPUData :: IO (Maybe CPUData)
getCPUData = do
  mMap <- parseKeyValuePairFile "/proc/cpuinfo" ["model name"]
  case mMap of
    Nothing -> do
      putStrLn "Couldn't get cpuinfo"
      pure $
        CPUData <$> Just "-"
    Just cpuDataMap -> do
      pure $
        CPUData
          <$> cpuDataMap Map.!? "model name"
