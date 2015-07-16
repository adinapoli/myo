{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo where

import qualified Language.C.Inline as C

import Myo.Foreign.Types
import Foreign.C.String
import Data.Word

C.context myoCtx
C.include "libmyo.h"
C.include "wrapper.h"

-------------------------------------------------------------------------------
fromMyoString :: MyoString -> CString
fromMyoString sPtr = [C.pure| const char* { libmyo_string_c_str($(libmyo_string_t sPtr)) } |]

-------------------------------------------------------------------------------
macAddressToString :: Word64 -> MyoString
macAddressToString i = [C.pure| libmyo_string_t { libmyo_mac_address_to_string($(uint64_t i)) } |]

-------------------------------------------------------------------------------
stringToMacAddress :: CString -> Word64
stringToMacAddress sPtr = [C.pure| uint64_t { libmyo_string_to_mac_address($(const char* sPtr)) } |]

-------------------------------------------------------------------------------
-- | Free the resources allocated by the string object.
myoStringFree :: MyoString -> IO ()
myoStringFree ms = [C.exp| void { libmyo_string_free($(libmyo_string_t ms))}|]

-------------------------------------------------------------------------------
errorCString :: ErrorDetails -> CString
errorCString ed = [C.pure| const char* { libmyo_error_cstring($(libmyo_error_details_t ed)) }|]
