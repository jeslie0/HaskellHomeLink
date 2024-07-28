module PipeWire.Spa.Utils.Dict (SpaDictItem, spaDictItem) where

import Foreign.C (CUInt, CChar)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.Storable (Storable(..))
import Foreign (Ptr, ForeignPtr(..), withForeignPtr, mallocForeignPtr)
import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import System.IO.Unsafe (unsafePerformIO)

#include <spa/utils/dict.h>

data SpaDictItem_t = SpaDictItem_t { _key :: ConstPtr CChar, _value :: ConstPtr CChar } deriving (Eq, Show)

instance Storable SpaDictItem_t where
  sizeOf _ = (#size struct spa_dict_item)

  alignment _ = (#alignment struct spa_dict_item)

  peek ptr = SpaDictItem_t
             <$> (#peek struct spa_dict_item, key) ptr
             <*> (#peek struct spa_dict_item, value) ptr

  poke ptr (SpaDictItem_t keyPtr valuePtr) = do
    pokeByteOff ptr 0 keyPtr
    pokeByteOff ptr (sizeOf keyPtr) valuePtr


newtype SpaDictItem = SpaDictItem (ForeignPtr SpaDictItem_t)

{-# NOINLINE spaDictItem #-}
spaDictItem :: T.Text -> T.Text -> SpaDictItem
spaDictItem key value = SpaDictItem . unsafePerformIO $
  T.withCString key
  (\keyPtr -> T.withCString value
              (\valPtr -> do
                  frnPtr :: ForeignPtr SpaDictItem_t <- mallocForeignPtr
                  withForeignPtr frnPtr (\ptr -> poke ptr $ SpaDictItem_t (ConstPtr keyPtr) (ConstPtr valPtr))
                  return frnPtr)
  )


data SpaDict_t = SpaDict_t { _flags :: CUInt, _n_items :: CUInt, _items :: Ptr SpaDictItem_t } deriving (Eq, Show)


instance Storable SpaDict_t where
  sizeOf _ = (#size struct spa_dict)

  alignment _ = (#alignment struct spa_dict)

  peek ptr = SpaDict_t
             <$> (#peek struct spa_dict, flags) ptr
             <*> (#peek struct spa_dict, n_items) ptr
             <*> (#peek struct spa_dict, items) ptr

  poke ptr (SpaDict_t _flags _n_items _items) = do
    pokeByteOff ptr 0 _flags
    pokeByteOff ptr (#size uint32_t) _n_items
    pokeByteOff ptr ((#size uint32_t) + (#size struct spa_dict_item*)) _items


newtype SpaDict = SpaDict (ForeignPtr SpaDict_t)

-- {-# NOINLINE spaDict #-}
-- spaDict :: Word -> [SpaDictItem] -> SpaDict
-- spaDict flags items  = SpaDict . unsafePerformIO $ do
--   frnPtr :: ForeignPtr SpaDict_t <- mallocForeignPtr
--   withForeignPtr frnPtr
--     (\ptr -> do
--         frnArrayPtr :: ForeignPtr
--         poke ptr $ SpaDict_t (fromIntegral flags) (length items) (un)
--         )
