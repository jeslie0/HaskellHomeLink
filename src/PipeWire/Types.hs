{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PipeWire.Types where

import Foreign (ForeignPtr)
import Foreign.C (CUInt (..), CString)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (Ptr)
import Foreign.Ptr (FunPtr)
import PipeWire.Spa.Utils.Dict
import Foreign.ForeignPtr (mallocForeignPtr, withForeignPtr)
import Foreign.C.String (newCString)
import Foreign (Storable(..))

-- foreign import capi unsafe "types.h check_spa_item" check_spa_item_c :: Ptr SpaDictItem -> IO ()


-- data SpaDictItem_t

-- newtype SpaDictItem = SpaDictItem (ForeignPtr SpaDictItem_t)

-- foreign import capi unsafe "types.h new_spa_dict_item" new_spa_dict_item_c :: CString -> CString -> IO (Ptr SpaDictItem_t)

-- foreign import capi unsafe "types.h &free_spa_dict_item" free_spa_dict_item_c :: FunPtr (Ptr SpaDictItem_t -> IO ())

-- data SpaDict_t

-- newtype SpaDict = SpaDict (ForeignPtr SpaDict_t)

-- foreign import capi unsafe "types.h new_spa_dict" new_spa_dict_c :: CUInt -> CUInt -> ConstPtr SpaDictItem_t -> IO (Ptr SpaDict_t)

-- foreign import capi unsafe "types.h &free_spa_dict" free_spa_dict_c :: FunPtr (Ptr SpaDict_t -> IO ())
