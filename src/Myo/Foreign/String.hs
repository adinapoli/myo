{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Myo.Foreign.String where

import Foreign.C

import Myo.Foreign.Types
import Data.Word

import qualified Language.C.Inline as C

C.context myoCtx
C.include "libmyo.h"
C.include "wrapper.h"

fromMyoString :: MyoString -> IO CString
fromMyoString sPtr = [C.exp| const char* { libmyo_string_c_str($(libmyo_string_t sPtr)) } |]

macAddressToString :: Word64 -> IO MyoString
macAddressToString i = [C.exp| libmyo_string_t { libmyo_mac_address_to_string($(uint64_t i)) } |]

stringToMacAddress :: CString -> IO Word64
stringToMacAddress sPtr = [C.exp| uint64_t { libmyo_string_to_mac_address($(const char* sPtr)) } |]

-- | Free the resources allocated by the string object.
myoStringFree :: MyoString -> IO ()
myoStringFree ms = [C.exp| void { libmyo_string_free($(libmyo_string_t ms))}|]
