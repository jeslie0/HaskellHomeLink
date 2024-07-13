{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PipeWire.PipeWireH (pw_init_c, pw_deinit_c, pw_debug_is_category_enabled_c, pw_get_application_name_c, pw_get_prgname_c, pw_get_user_name_c, pw_get_host_name_c, pw_get_client_name_c, pw_check_option_c, pw_set_domain_c, pw_get_domain_c) where

import Foreign.C (CBool (..), CChar, CInt (..), CString)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (Ptr, nullPtr)

-- | Initialize PipeWire.
foreign import capi unsafe "pipewire/pipewire.h pw_init" pw_init_c :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

-- | Deinitialize PipeWire.
foreign import capi unsafe "pipewire/pipewire.h pw_deinit" pw_deinit_c :: IO ()

-- | Check if a debug category is enabled.
foreign import capi unsafe "pipewire/pipewire.h pw_debug_is_category_enabled" pw_debug_is_category_enabled_c :: ConstPtr CChar -> IO CBool

-- | Get the application name.
foreign import capi unsafe "pipewire/pipewire.h pw_get_application_name" pw_get_application_name_c :: IO (ConstPtr CChar)

foreign import capi unsafe "pipewire/pipewire.h pw_get_prgname" pw_get_prgname_c :: IO (ConstPtr CChar)

foreign import capi unsafe "pipewire/pipewire.h pw_get_user_name" pw_get_user_name_c :: IO (ConstPtr CChar)

foreign import capi unsafe "pipewire/pipewire.h pw_get_host_name" pw_get_host_name_c :: IO (ConstPtr CChar)

foreign import capi unsafe "pipewire/pipewire.h pw_get_client_name" pw_get_client_name_c :: IO (ConstPtr CChar)

foreign import capi unsafe "pipewire/pipewire.h pw_check_option" pw_check_option_c :: ConstPtr CChar -> ConstPtr CChar -> IO CBool

-- foreign import capi unsafe "pipewire/pipewire.h pw_direction_reverse" pw_direction_reverse_c :: ConstPtr CChar -> ConstPtr CChar -> IO CBool

foreign import capi unsafe "pipewire/pipewire.h pw_set_domain" pw_set_domain_c :: ConstPtr CChar -> IO CInt

foreign import capi unsafe "pipewire/pipewire.h pw_get_domain" pw_get_domain_c :: IO (ConstPtr CChar)

-- foreign import capi unsafe "pipewire/pipewire.h pw_load_spa_handle" pw_load_spa_handle_c :: IO (ConstPtr CChar)

-- foreign import capi unsafe "pipewire/pipewire.h pw_unload_spa_handle" pw_unload_spa_handle_c :: IO (ConstPtr CChar)

foreign import capi unsafe "pipewire/pipewire.h pw_get_library_version" pw_get_library_version_c :: IO (ConstPtr CChar)

foreign import capi unsafe "pipewire/pipewire.h pw_get_headers_version" pw_get_headers_version_c :: IO (ConstPtr CChar)
