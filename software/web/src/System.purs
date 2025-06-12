module System where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.UInt (UInt)
import Proto.DeviceData.DeviceData as Proto
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

type DeviceDataR =
  ( cpuData :: Maybe CPUData
  , inDockerContainer :: Boolean
  , operatingSystemName :: String
  , architecture :: String
  , memTotalKb :: Maybe UInt
  )

newtype DeviceData = DeviceData (Record DeviceDataR)

data DeviceDataError
  = MissingCPUData
  | MissingPartOfCPUData CPUDataError
  | MissingInDockerContainer
  | MissingOperatingSystemName
  | MissingArchitecture
  | DeviceDataDeviceError DeviceError

instance FromMessage Proto.DeviceData DeviceData DeviceDataError where
  fromMessage (Proto.DeviceData msg) = do
    let
      inDockerContainer = fromMaybe false msg.inDockerContainer
      mCPUDataProt = msg.cpuData
    cpuData <- lmap MissingPartOfCPUData $ traverse fromMessage mCPUDataProt
    operatingSystemName <- toEither MissingOperatingSystemName msg.operatingSystemName
    architecture <- toEither MissingArchitecture msg.architecture
    memTotalKb <- Right $ msg.memTotalkB
    pure $ DeviceData { cpuData, inDockerContainer, operatingSystemName, architecture, memTotalKb }

instance SayError DeviceDataError where
  sayError MissingCPUData = pure "DeviceData is missing cpuData"
  sayError (MissingPartOfCPUData errs) = "DeviceData is missing cpuData" : sayError errs
  sayError MissingInDockerContainer = pure "DeviceData is missing inDockerContainer"
  sayError MissingOperatingSystemName = pure "DeviceData is missing operatingSystemName"
  sayError MissingArchitecture = pure "DeviceData is missing architecture"
  sayError (DeviceDataDeviceError _) = pure "DeviceData is missing device"

data Device
  = Home
  | Proxy
  | Camera
  | UnknownDevice

devices :: Array Device
devices = [ Home, Proxy, Camera ]

data DeviceError = InvalidDevice

derive instance eqDevice :: Eq Device
derive instance ordDevice :: Ord Device

instance Show Device where
  show Home = "Home"
  show Proxy = "Remote Proxy"
  show Camera = "Camera"
  show UnknownDevice = "Unknown"

instance FromMessage Proto.DEVICE Device DeviceError where
  fromMessage Proto.DEVICE_HOME = Right Home
  fromMessage Proto.DEVICE_PROXY = Right Proxy
  fromMessage Proto.DEVICE_CAMERA = Right Camera
  fromMessage Proto.DEVICE_UNKNOWN = Right UnknownDevice

type DeviceDataPairR = (device :: Device, deviceData :: DeviceData)
newtype DeviceDataPair = DeviceDataPair (Record DeviceDataPairR)
data DeviceDataPairError
  = MissingDevice
  | InvalidDeviceInDevicePair DeviceError
  | MissingDeviceData
  | InvalidDeviceDataInDevicePair DeviceDataError
instance SayError DeviceDataPairError where
  sayError MissingDevice = pure "Missing device"
  sayError (InvalidDeviceInDevicePair err) = Array.cons "Invalid device in device pair" (sayError err)
  sayError MissingDeviceData = pure "Missing device data"
  sayError (InvalidDeviceDataInDevicePair err) = Array.cons "Invalid device data in device data pair" (sayError err)

instance FromMessage Proto.DeviceDataPair DeviceDataPair DeviceDataPairError where
  fromMessage (Proto.DeviceDataPair pair) = do
    deviceProt <- toEither MissingDevice pair.device
    device <- lmap InvalidDeviceInDevicePair $ fromMessage deviceProt
    deviceDataProt <- toEither MissingDeviceData pair.deviceData
    deviceData <- lmap InvalidDeviceDataInDevicePair $ fromMessage deviceDataProt
    pure $ DeviceDataPair { device, deviceData: deviceData }

instance SayError DeviceError where
  sayError InvalidDevice = pure "Device error: Invalid device"

type AllDeviceDataR = (allDeviceData :: Array DeviceDataPair)
newtype AllDeviceData = AllDeviceData (Record AllDeviceDataR)

data AllDeviceDataError
  = MissingAllDeviceData
  | MissingPartOfAllDeviceData DeviceDataPairError

instance FromMessage Proto.AllDeviceData AllDeviceData AllDeviceDataError where
  fromMessage (Proto.AllDeviceData msg) =
    AllDeviceData <<< { allDeviceData: _ }
      <$> (lmap MissingPartOfAllDeviceData <<< traverse fromMessage $ msg.allDeviceData)


instance SayError AllDeviceDataError where
  sayError MissingAllDeviceData = pure "AllDeviceData is missing allDeviceData"
  sayError (MissingPartOfAllDeviceData err) = "DeviceDeviceData has invalid allDeviceData" : sayError err

type DeviceMemoryInformationR = (device :: Device, timeMem :: Array (Array Number))
newtype DeviceMemoryInformation = DeviceMemoryInformation (Record DeviceMemoryInformationR)
data DeviceMemoryInformationError
  = MissingMemDevice
  | InvalidMemDevice DeviceError
  | MissingMemoryInformation

instance FromMessage Proto.DeviceMemoryInformation DeviceMemoryInformation DeviceMemoryInformationError where
  fromMessage (Proto.DeviceMemoryInformation msg) = do
    deviceProt <- toEither MissingMemDevice (msg.device)
    device <- lmap InvalidMemDevice $ fromMessage deviceProt
    pure $ DeviceMemoryInformation { device, timeMem: msg.timeMem <#> \(Proto.MemoryInformation info) -> info.pair }

type AllDeviceMemoryDataR = (allDeviceMemoryData :: Array DeviceMemoryInformation)
newtype AllDevicesMemoryData = AllDevicesMemoryData (Record AllDeviceMemoryDataR)
data AllDevicesMemoryDataError
  = MissingAllDevicesMemoryData
  | MissingPartOfAllMemoryData (Array DeviceMemoryInformationError)
  | Foo DeviceMemoryInformationError

instance FromMessage Proto.AllDeviceMemoryData AllDevicesMemoryData AllDevicesMemoryDataError where
  fromMessage (Proto.AllDeviceMemoryData msg) = do
    foo <- lmap MissingPartOfAllMemoryData $
      ( traverse
          (\e -> lmap (\_ -> []) e)
          (fromMessage @Proto.DeviceMemoryInformation @DeviceMemoryInformation <$> msg.allDeviceMemoryData)
      )
    pure $ AllDevicesMemoryData { allDeviceMemoryData: foo }
