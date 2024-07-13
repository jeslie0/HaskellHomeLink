module PipeWire (withPipeWire, debugIsCategoryEnabled, getApplicationName, getProgramName, getUserName, getClientName, getHostName, checkOption, getDomain, setDomain) where

import Control.Exception (bracket_)
import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (nullPtr)
import Lib (cBoolToBool, cStringToText)
import PipeWire.PipeWireH (pw_check_option_c, pw_debug_is_category_enabled_c, pw_deinit_c, pw_get_application_name_c, pw_get_client_name_c, pw_get_domain_c, pw_get_host_name_c, pw_get_prgname_c, pw_get_user_name_c, pw_init_c, pw_set_domain_c)

-- | Run an IO action after initialising PipeWire and deinitialize
-- after completion..
withPipeWire :: IO a -> IO a
withPipeWire =
  bracket_ (pw_init_c nullPtr nullPtr) pw_deinit_c

-- | Check if a debug category is enabled.
debugIsCategoryEnabled :: T.Text -> IO Bool
debugIsCategoryEnabled txt =
  T.withCString txt $ \cString -> cBoolToBool <$> pw_debug_is_category_enabled_c (ConstPtr cString)

-- | Get the application name.
getApplicationName :: IO (Maybe T.Text)
getApplicationName = do
  ConstPtr cString <- pw_get_application_name_c
  cStringToText cString

-- | Get the program name.
getProgramName :: IO (Maybe T.Text)
getProgramName = do
  ConstPtr cString <- pw_get_prgname_c
  cStringToText cString

-- | Get the user name.
getUserName :: IO (Maybe T.Text)
getUserName = do
  ConstPtr cString <- pw_get_user_name_c
  cStringToText cString

-- | Get the host name.
getHostName :: IO (Maybe T.Text)
getHostName = do
  ConstPtr cString <- pw_get_host_name_c
  cStringToText cString

-- | Get the user name.
getClientName :: IO (Maybe T.Text)
getClientName = do
  ConstPtr cString <- pw_get_client_name_c
  cStringToText cString

checkOption :: T.Text -> T.Text -> IO Bool
checkOption option value =
  T.withCString option $
    \cOption -> T.withCString value $
      \cValue ->
        cBoolToBool <$> pw_check_option_c (ConstPtr cOption) (ConstPtr cValue)

setDomain :: T.Text -> IO Int
setDomain domain =
  T.withCString domain $
    \cDomain -> fromIntegral <$> pw_set_domain_c (ConstPtr cDomain)

getDomain :: IO (Maybe T.Text)
getDomain = do
  ConstPtr cString <- pw_get_domain_c
  cStringToText cString
