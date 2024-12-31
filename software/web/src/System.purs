module System where

import Prelude

import Data.Array (null, (:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Enum (class Enum, Cardinality(..), fromEnum, toEnum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Proto.Messages as Proto
import Protobuf.Internal.Prelude (class BoundedEnum)

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
    inDockerContainer <- toEither MissingInDockerContainer msg.inDockerContainer
    pure $ SystemData { cpuData, inDockerContainer }

instance SayError SystemDataError where
  sayError MissingCPUData = pure "SystemData is missing cpuData"
  sayError (MissingPartOfCPUData errs) = "SystemData is missing cpuData" : sayError errs
  sayError MissingInDockerContainer = pure "SystemData is missing inDockerContainer"

data Island
  = Home
  | LocalProxy
  | RemoteProxy

derive instance eqIsland :: Eq Island
derive instance ordIsland :: Ord Island

instance Show Island where
  show Home = "Home"
  show LocalProxy = "Local Proxy"
  show RemoteProxy = "Remote Proxy"

instance Enum Island where
  succ Home = Just LocalProxy
  succ LocalProxy = Just RemoteProxy
  succ RemoteProxy = Nothing

  pred Home = Nothing
  pred LocalProxy = Just Home
  pred RemoteProxy = Just LocalProxy

instance Bounded Island where
  bottom = Home
  top = RemoteProxy

instance BoundedEnum Island where
  cardinality = Cardinality 3

  toEnum 1 = Just Home
  toEnum 2 = Just LocalProxy
  toEnum 3 = Just RemoteProxy
  toEnum _ = Nothing

  fromEnum Home = 1
  fromEnum LocalProxy = 2
  fromEnum RemoteProxy = 3

type IslandSystemDataR = (island :: Island, systemData :: SystemData)
newtype IslandSystemData = IslandSystemData (Record IslandSystemDataR)
data IslandSystemDataError
  = MissingIsland
  | InvalidIsland
  | MissingSystemData
  | MissingPartOfSystemData SystemDataError

instance FromMessage Proto.IslandSystemData IslandSystemData IslandSystemDataError where
  fromMessage (Proto.IslandSystemData msg) = do
    island' <- toEither MissingIsland msg.island
    island <- toEither InvalidIsland $ toEnum (fromEnum island')
    systemDataProt <- toEither MissingSystemData msg.systemData
    systemData <- lmap MissingPartOfSystemData $ fromMessage systemDataProt
    pure $ IslandSystemData { systemData, island }

instance SayError IslandSystemDataError where
  sayError MissingIsland = pure "IslandSystemData is missing island"
  sayError InvalidIsland = pure "IslandSystemData has invalid island"
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
