{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Myo.Foreign.String where

import Foreign.Ptr
import Foreign.C

import Myo.Foreign.Types
import Data.Word

import qualified Language.C.Inline as C

C.context myoCtx
C.include "libmyo.h"
C.include "wrapper.h"

fromMyoString :: Ptr MyoString -> IO CString
fromMyoString sPtr = [C.exp| const char* { libmyo_string_c_str($(libmyo_string_t *sPtr)) } |]

macAddressToString :: Word64 -> IO (Ptr MyoString)
macAddressToString i = [C.exp| libmyo_string_t* { (libmyo_string_t*)(libmyo_mac_address_to_string($(uint64_t i))) } |]

stringToMacAddress :: CString -> IO Word64
stringToMacAddress sPtr = [C.exp| uint64_t { libmyo_string_to_mac_address($(const char* sPtr)) } |]

-- // Free the resources allocated by the string object.
-- LIBMYO_EXPORT
-- void libmyo_string_free(libmyo_string_t);
