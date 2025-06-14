module Devices (Device (..), devices, proxies) where

import Control.Monad (void)
import Data.Serialize (Serialize (..), ensure, getWord8, putWord8)
import GHC.Generics (Generic)
import Proto.DeviceData qualified as Proto
import ProtoHelper (FromMessage (..), ToMessage (..))

data Device
  = Unknown
  | Home
  | Proxy
  | Camera
  deriving (Generic, Eq, Ord, Enum, Show)

instance Serialize Device where
  get = do
    void $ ensure 1
    toEnum . fromIntegral <$> getWord8

  put = putWord8 . fromIntegral . fromEnum

instance FromMessage Proto.DEVICE Device where
  fromMessage = toEnum . fromEnum

instance ToMessage Proto.DEVICE Device where
  toMessage = toEnum . fromEnum

devices :: [Device]
devices =
  [Home, Proxy, Camera]

proxies :: [Device]
proxies = [Proxy]
