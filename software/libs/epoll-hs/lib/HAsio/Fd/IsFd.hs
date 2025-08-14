module HAsio.Fd.IsFd where

import System.Posix (Fd)

-- | Represents types that use a file descriptor under the hood.
class IsFd fd where
  -- | Convert to a file descriptor.
  toFd :: fd -> Fd

  -- | Convert from a file descriptor.
  fromFd :: Fd -> fd

instance IsFd Fd where
  toFd = id

  fromFd = id
