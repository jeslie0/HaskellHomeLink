module HAsio.Fd.IsFd where

import Foreign.C.Types (CInt)
import System.Posix (Fd (Fd))

-- | Represents types that use a file descriptor under the hood.
class IsFd fd where
  -- | Convert to a file descriptor.
  toFd :: fd -> Fd

  -- | Convert from a file descriptor.
  fromFd :: Fd -> fd

instance IsFd Fd where
  toFd = id

  fromFd = id

instance IsFd CInt where
  toFd = Fd

  fromFd (Fd n) = n
