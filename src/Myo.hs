{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo (
    module Myo.Foreign.Types
  , module Myo.Foreign.Hub
  , fromMyoString
  , macAddressToString
  , stringToMacAddress
  , myoStringFree
  , errorKind
  , freeErrorDetails
  , errorCString
) where

import qualified Language.C.Inline as C

import Myo.Foreign.Types
import Myo.Foreign.Hub
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Storable
import Data.Word
import System.IO.Unsafe

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
errorKind :: ErrorDetails -> IO Result
errorKind ed = withForeignPtr ed $ \ed' -> do
  v <- [C.block| libmyo_result_t* {
    libmyo_result_t* res;
    *res = libmyo_error_kind(&$(libmyo_error_details_t* ed'));
    return res;
    }
    |]
  peek v

-------------------------------------------------------------------------------
-- TODO: Make this a FunPtr
freeErrorDetails :: ErrorDetails -> IO ()
freeErrorDetails ed = withForeignPtr ed $ \ed' -> do
  [C.exp| void { libmyo_free_error_details(&$(libmyo_error_details_t* ed')) } |]

-------------------------------------------------------------------------------
errorCString :: ErrorDetails -> CString
errorCString ed = unsafePerformIO $ withForeignPtr ed $ \ed' -> do
 [C.exp| const char* { libmyo_error_cstring(&$(libmyo_error_details_t* ed')) }|]
