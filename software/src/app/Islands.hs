module Islands (Island (..), islands, proxies) where

import Control.Monad (void)
import Data.Serialize (Serialize (..), getWord8, ensure, putWord8)
import GHC.Generics (Generic)
import Proto.Messages qualified as Proto
import ProtoHelper (FromMessage (..), ToMessage (..))

data Island
  = Home
  | RemoteProxy
  | LocalHTTP
  | Unknown
  deriving (Generic, Eq, Ord, Enum, Show)

instance FromMessage Proto.ISLAND Island where
  fromMessage Proto.HOME = Home
  fromMessage Proto.LOCAL_HTTP = LocalHTTP
  fromMessage Proto.REMOTE_PROXY = RemoteProxy
  fromMessage _ = Unknown

instance ToMessage Proto.ISLAND Island where
  toMessage Home = Proto.HOME
  toMessage LocalHTTP = Proto.LOCAL_HTTP
  toMessage RemoteProxy = Proto.REMOTE_PROXY
  toMessage Unknown = Proto.UNKNOWN

islands :: [Island]
islands =
  [Home, RemoteProxy, LocalHTTP]

proxies :: [Island]
proxies = [RemoteProxy, LocalHTTP]

instance Serialize Island where
  get = do
    void $ ensure 1
    toEnum . fromIntegral <$> getWord8

  put = putWord8 . fromIntegral . fromEnum
