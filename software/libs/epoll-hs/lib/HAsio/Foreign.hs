{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module HAsio.Foreign where

import Data.Bits ((.|.))
import Data.Foldable (foldl')
import Data.Void (Void)
import Foreign (Ptr)
import Foreign.C (CInt (..), CSize (..))
import Foreign.C.String (CString)
import System.Posix (CSsize (..))

-- * Foreign imports

-- ** Open

foreign import capi unsafe "fcntl.h open"
  c_open_unsafe :: CString -> CInt -> CInt -> IO CInt

foreign import capi safe "fcntl.h open"
  c_open :: CString -> CInt -> CInt -> IO CInt

-- ** Read

foreign import capi unsafe "unistd.h read"
  c_read_unsafe :: CInt -> Ptr Void -> CSize -> IO CSsize

foreign import capi safe "unistd.h read"
  c_read :: CInt -> Ptr Void -> CSize -> IO CSsize

-- ** Write

foreign import capi unsafe "unistd.h write"
  c_write_unsafe :: CInt -> Ptr Void -> CSize -> IO CSsize

foreign import capi safe "unistd.h write"
  c_write :: CInt -> Ptr Void -> CSize -> IO CSsize

-- ** Close

foreign import capi unsafe "unistd.h close"
  c_close_unsafe :: CInt -> IO CInt

foreign import capi safe "unistd.h close"
  c_close :: CInt -> IO CInt

-- foreign import capi unsafe "fcntl.h openat"
--   c_openat_unsafe :: Int -> CString -> CInt -> CInt -> IO CInt

-- foreign import capi safe "fcntl.h openat"
--   c_openat :: Int -> CString -> CInt -> CInt -> IO CInt

-- foreign import capi safe "fcntl.h creat"
--   c_creat :: CString -> CInt -> IO CInt

-- foreign import capi unsafe "fcntl.h creat"
--   c_creat_unsafe :: CString -> CInt -> IO CInt

-- ** Flags

foreign import capi "fcntl.h value O_APPEND"
  c_O_APPEND :: CInt

foreign import capi "fcntl.h value O_ASYNC"
  c_O_ASYNC :: CInt

foreign import capi "fcntl.h value O_CLOEXEC"
  c_O_CLOEXEC :: CInt

foreign import capi "fcntl.h value O_CREAT"
  c_O_CREAT :: CInt

foreign import capi "fcntl.h value O_DIRECT"
  c_O_DIRECT :: CInt

foreign import capi "fcntl.h value O_DIRECTORY"
  c_O_DIRECTORY :: CInt

foreign import capi "fcntl.h value O_DSYNC"
  c_O_DSYNC :: CInt

foreign import capi "fcntl.h value O_EXCL"
  c_O_EXCL :: CInt

foreign import capi "fcntl.h value O_LARGEFILE"
  c_O_LARGEFILE :: CInt

foreign import capi "fcntl.h value O_NOATIME"
  c_O_NOATIME :: CInt

foreign import capi "fcntl.h value O_NOCTTY"
  c_O_NOCTTY :: CInt

foreign import capi "fcntl.h value O_NOFOLLOW"
  c_O_NOFOLLOW :: CInt

foreign import capi "fcntl.h value O_NONBLOCK"
  c_O_NONBLOCK :: CInt

foreign import capi "fcntl.h value O_NDELAY"
  c_O_NDELAY :: CInt

foreign import capi "fcntl.h value O_PATH"
  c_O_PATH :: CInt

foreign import capi "fcntl.h value O_SYNC"
  c_O_SYNC :: CInt

foreign import capi "fcntl.h value O_TMPFILE"
  c_O_TMPFILE :: CInt

foreign import capi "fcntl.h value O_TRUNC"
  c_O_TRUNC :: CInt

data Flag
  = Append
  | Async
  | CloseExec
  | Create
  | Direct
  | Directory
  | DSync
  | Excl
  | LargeFile
  | NoAtime
  | NoCTTY
  | NoFollow
  | NonBlock
  | NDelay
  | Path
  | Sync
  | TmpFile
  | Trunc
  deriving (Eq, Show, Ord, Enum)

flagToCInt :: Flag -> CInt
flagToCInt flag = case flag of
  Append -> c_O_APPEND
  Async -> c_O_ASYNC
  CloseExec -> c_O_CLOEXEC
  Create -> c_O_CREAT
  Direct -> c_O_DIRECT
  Directory -> c_O_DIRECTORY
  DSync -> c_O_DSYNC
  Excl -> c_O_EXCL
  LargeFile -> c_O_LARGEFILE
  NoAtime -> c_O_NOATIME
  NoCTTY -> c_O_NOCTTY
  NoFollow -> c_O_NOFOLLOW
  NonBlock -> c_O_NONBLOCK
  NDelay -> c_O_NDELAY
  Path -> c_O_PATH
  Sync -> c_O_SYNC
  TmpFile -> c_O_TMPFILE
  Trunc -> c_O_TRUNC

flagsToCInt :: Foldable f => f Flag -> CInt
flagsToCInt =
  foldl' (\acc prev -> flagToCInt prev .|. acc) 0
