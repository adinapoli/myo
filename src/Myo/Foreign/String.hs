{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Myo.Foreign.String where

import qualified Language.C.Inline as C

import Myo.Foreign.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word

C.context myoCtx
C.include "libmyo.h"
C.include "wrapper.h"

-------------------------------------------------------------------------------
fromMyoString :: MyoString -> IO CString
fromMyoString sPtr = withForeignPtr sPtr $ \ms -> do
  [C.exp| const char* { libmyo_string_c_str($(libmyo_string_t ms)) } |]

-------------------------------------------------------------------------------
macAddressToString :: Word64 -> IO MyoString
macAddressToString i = do
  newStr <- [C.exp| libmyo_string_t {
    libmyo_mac_address_to_string($(uint64_t i))
    }
  |]
  newForeignPtr freeMyoString newStr

-------------------------------------------------------------------------------
stringToMacAddress :: CString -> Word64
stringToMacAddress sPtr = [C.pure| uint64_t { libmyo_string_to_mac_address($(const char* sPtr)) } |]

-------------------------------------------------------------------------------
-- | Free the resources allocated by the string object.
foreign import ccall "wrapper.h &myo_string_free"
  freeMyoString :: FunPtr (Ptr MyoString_t -> IO ())
