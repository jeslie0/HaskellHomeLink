module System where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.UInt (UInt)
import Debug (trace)
import Proto.Messages as Proto
import ProtoHelper (class FromMessage, class SayError, fromMessage, sayError, toEither)

type CPUDataR = (modelName :: String)
newtype CPUData = CPUData (Record CPUDataR)

data CPUDataError = MissingModelName

instance FromMessage Proto.CPUData CPUData CPUDataError where
  fromMessage (Proto.CPUData msg) = ado
    modelName <- toEither MissingModelName msg.modelName
    in CPUData { modelName }

instance SayError CPUDataError where
  sayError MissingModelName = pure "Model name is missing from CPUData"

type SystemDataR =
  ( cpuData :: Maybe CPUData
  , inDockerContainer :: Boolean
  , operatingSystemName :: String
  , architecture :: String
  , memTotalKb :: Maybe UInt
  , island :: Island
  )

newtype SystemData = SystemData (Record SystemDataR)

data SystemDataError
  = MissingCPUData
  | MissingPartOfCPUData CPUDataError
  | MissingInDockerContainer
  | MissingOperatingSystemName
  | MissingArchitecture
  | SystemDataMissingIsland
  | SystemDataIslandError IslandError

instance FromMessage Proto.SystemData SystemData SystemDataError where
  fromMessage (Proto.SystemData msg) = do
    let inDockerContainer = fromMaybe false msg.inDockerContainer
        mCPUDataProt = msg.cpuData
    cpuData <- lmap MissingPartOfCPUData $ traverse fromMessage mCPUDataProt
    operatingSystemName <- toEither MissingOperatingSystemName msg.operatingSystemName
    architecture <- toEither MissingArchitecture msg.architecture
    memTotalKb <- Right $ msg.memTotalkB
    islandProt <- toEither SystemDataMissingIsland msg.island
    island <- lmap SystemDataIslandError $ fromMessage islandProt
    pure $ SystemData { cpuData, inDockerContainer, operatingSystemName, architecture, memTotalKb, island }

instance SayError SystemDataError where
  sayError MissingCPUData = pure "SystemData is missing cpuData"
  sayError (MissingPartOfCPUData errs) = "SystemData is missing cpuData" : sayError errs
  sayError MissingInDockerContainer = pure "SystemData is missing inDockerContainer"
  sayError MissingOperatingSystemName = pure "SystemData is missing operatingSystemName"
  sayError MissingArchitecture = pure "SystemData is missing architecture"
  sayError SystemDataMissingIsland = pure "SystemData is missing island"
  sayError (SystemDataIslandError _) = pure "SystemData is missing island"

data Island
  = Home
  | LocalHTTP
  | RemoteProxy
  | UnknownIsland

islands :: Array Island
islands = [ Home, LocalHTTP, RemoteProxy ]

data IslandError = InvalidIsland

derive instance eqIsland :: Eq Island
derive instance ordIsland :: Ord Island

instance Show Island where
  show Home = "Home"
  show LocalHTTP = "Local Proxy"
  show RemoteProxy = "Remote Proxy"
  show UnknownIsland = "Unknown"

instance FromMessage Proto.ISLAND Island IslandError where
  fromMessage Proto.ISLAND_HOME = Right Home
  fromMessage Proto.ISLAND_LOCAL_HTTP = Right LocalHTTP
  fromMessage Proto.ISLAND_REMOTE_PROXY = Right RemoteProxy
  fromMessage Proto.ISLAND_UNKNOWN = Right UnknownIsland

instance SayError IslandError where
  sayError InvalidIsland = pure "Island error: Invalid island"

type IslandsSystemDataR = (allSystemData :: Array SystemData)
newtype IslandsSystemData = IslandsSystemData (Record IslandsSystemDataR)

data IslandsSystemDataError
  = MissingIslandsSystemData
  | MissingPartOfAllSystemData (Array SystemDataError)

instance FromMessage Proto.IslandsSystemData IslandsSystemData IslandsSystemDataError where
  fromMessage (Proto.IslandsSystemData msg) =
    IslandsSystemData <<< { allSystemData: _ }
      <$>
        ( lmap MissingPartOfAllSystemData
            $ traverse (lmap pure <<< fromMessage) (msg.allSystemData)
        )

instance SayError IslandsSystemDataError where
  sayError MissingIslandsSystemData = pure "IslandsSystemData is missing allSystemData"
  sayError (MissingPartOfAllSystemData err) = "IslandSystemData has invalid allSystemData" : Array.concatMap sayError err

type IslandMemoryInformationR = (island :: Island, timeMem :: Array (Array Number))
newtype IslandMemoryInformation = IslandMemoryInformation (Record IslandMemoryInformationR)
data IslandMemoryInformationError
  = MissingMemIsland
  | InvalidMemIsland IslandError
  | MissingMemoryInformation

instance FromMessage Proto.IslandMemoryInformation IslandMemoryInformation IslandMemoryInformationError where
  fromMessage (Proto.IslandMemoryInformation msg) = do
    islandProt <- toEither MissingMemIsland (msg.island)
    island <- lmap InvalidMemIsland $ fromMessage islandProt
    pure $ IslandMemoryInformation { island, timeMem: msg.timeMem <#> \(Proto.MemoryInformation info) -> info.pair }

type AllIslandMemoryDataR = (allIslandMemoryData :: Array IslandMemoryInformation)
newtype AllIslandsMemoryData = AllIslandsMemoryData (Record AllIslandMemoryDataR)
data AllIslandsMemoryDataError
  = MissingAllIslandsMemoryData
  | MissingPartOfAllMemoryData (Array IslandMemoryInformationError)
  | Foo IslandMemoryInformationError

instance FromMessage Proto.AllIslandMemoryData AllIslandsMemoryData AllIslandsMemoryDataError where
  fromMessage (Proto.AllIslandMemoryData msg) = do
    foo <- lmap MissingPartOfAllMemoryData $
      ( traverse
          (\e -> lmap (\_ -> []) e)
          (fromMessage @Proto.IslandMemoryInformation @IslandMemoryInformation <$> msg.allIslandMemoryData)
      )
    pure $ AllIslandsMemoryData { allIslandMemoryData: foo }
