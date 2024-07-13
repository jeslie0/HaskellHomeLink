module Lib where

import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Foreign.C (CChar, CString)
import Foreign.Storable (peekByteOff)
import Foreign.C.Types (CBool(..))
import Data.Word8 as W
import Foreign.Ptr (nullPtr)

-- | Convert a CString to Text.
cStringToText :: CString -> IO (Maybe T.Text)
cStringToText cString = do
  mCStringLen <- findCStringLen
  case mCStringLen of
    Nothing -> return Nothing
    Just cStringLen ->  do
      m <- cStringLen
      txt <- T.peekCStringLen (cString, m)
      return $ Just txt
  where
    {-# INLINE findCStringLen #-}
    findCStringLen = if cString == nullPtr then return Nothing else return . Just $ go 0
      where
        go n = do
          char :: CChar <- peekByteOff cString n
          if char == 0 then return n else go (n + 1)

cBoolToBool :: CBool -> Bool
cBoolToBool (CBool n) = n /= W._0
