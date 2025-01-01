module System where

import Prelude

import Data.Array (null, (:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Proto.Messages as Proto

toEither :: forall a b. b -> Maybe a -> Either b a
toEither _ (Just a) = Right a
toEither b _ = Left b

class FromMessage msg type_ err | msg -> err where
  fromMessage :: msg -> Either err type_

class SayError err where
  sayError :: err -> Array String

type CPUDataR = (vendor :: String, modelName :: String)
newtype CPUData = CPUData (Record CPUDataR)

data CPUDataError
  = MissingVendor
  | MissingModelName

instance FromMessage Proto.CPUData CPUData CPUDataError where
  fromMessage (Proto.CPUData msg) = ado
    vendor <- toEither MissingVendor msg.vendor
    modelName <- toEither MissingModelName msg.vendor
    in CPUData { vendor, modelName }

instance SayError CPUDataError where
  sayError MissingVendor = pure "Vendor is missing from CPUData"
  sayError MissingModelName = pure "Model name is missing from CPUData"

type SystemDataR = (cpuData :: CPUData, inDockerContainer :: Boolean)
newtype SystemData = SystemData (Record SystemDataR)
data SystemDataError
  = MissingCPUData
  | MissingPartOfCPUData CPUDataError
  | MissingInDockerContainer

instance FromMessage Proto.SystemData SystemData SystemDataError where
  fromMessage (Proto.SystemData msg) = do
    cpuDataProt <- toEither MissingCPUData msg.cpuData
    cpuData <- lmap MissingPartOfCPUData $ fromMessage cpuDataProt
    let inDockerContainer = fromMaybe false msg.inDockerContainer
    pure $ SystemData { cpuData, inDockerContainer }

instance SayError SystemDataError where
  sayError MissingCPUData = pure "SystemData is missing cpuData"
  sayError (MissingPartOfCPUData errs) = "SystemData is missing cpuData" : sayError errs
  sayError MissingInDockerContainer = pure "SystemData is missing inDockerContainer"

data Island
  = Home
  | LocalHTTP
  | RemoteProxy

data IslandError = InvalidIsland

derive instance eqIsland :: Eq Island
derive instance ordIsland :: Ord Island

instance Show Island where
  show Home = "Home"
  show LocalHTTP = "Local Proxy"
  show RemoteProxy = "Remote Proxy"

instance FromMessage Proto.ISLAND Island IslandError where
  fromMessage Proto.ISLAND_HOME = Right Home
  fromMessage Proto.ISLAND_LOCAL_HTTP = Right LocalHTTP
  fromMessage Proto.ISLAND_REMOTE_PROXY = Right RemoteProxy

instance SayError IslandError where
  sayError InvalidIsland = pure "Island error: Invalid island"

type IslandSystemDataR = (island :: Island, systemData :: SystemData)
newtype IslandSystemData = IslandSystemData (Record IslandSystemDataR)
data IslandSystemDataError
  = MissingIsland
  | SystemDataInvalidIsland IslandError
  | MissingSystemData
  | MissingPartOfSystemData SystemDataError

instance FromMessage Proto.IslandSystemData IslandSystemData IslandSystemDataError where
  fromMessage (Proto.IslandSystemData msg) = do
    let island' = fromMaybe Proto.ISLAND_HOME msg.island
    island <- lmap SystemDataInvalidIsland $ fromMessage island'
    systemDataProt <- toEither MissingSystemData msg.systemData
    systemData <- lmap MissingPartOfSystemData $ fromMessage systemDataProt
    pure $ IslandSystemData { systemData, island }

instance SayError IslandSystemDataError where
  sayError MissingIsland = pure "IslandSystemData is missing island"
  sayError (SystemDataInvalidIsland err) = "IslandSystemData has invalid island" : sayError err
  sayError MissingSystemData = pure "IslandSystemData is missing systemData"
  sayError (MissingPartOfSystemData err) = "IslandSystemData has invalid systemData" : sayError err

type IslandsSystemDataR = (allSystemData :: Array IslandSystemData)
newtype IslandsSystemData = IslandsSystemData (Record IslandsSystemDataR)

data IslandsSystemDataError
  = MissingIslandsSystemData
  | MissingPartOfAllSystemData (Array IslandSystemDataError)

instance FromMessage Proto.IslandsSystemData IslandsSystemData IslandsSystemDataError where
  fromMessage (Proto.IslandsSystemData msg) =
    if null msg.allSystemData then Left MissingIslandsSystemData
    else IslandsSystemData <<< { allSystemData: _ }
      <$>
        ( lmap MissingPartOfAllSystemData
            $ traverse (lmap pure <<< fromMessage) (msg.allSystemData)
        )

instance SayError IslandsSystemDataError where
  sayError MissingIslandsSystemData = pure "IslandsSystemData is missing allSystemData"
  sayError (MissingPartOfAllSystemData err) = "IslandSystemData has invalid allSystemData" : Array.concatMap sayError err
