{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo (
    module Myo.Foreign.Types
  , module Myo.Foreign.Hub
  , module Myo.Foreign.String
  , errorKind
  , freeErrorDetails
  , errorCString
  , getMacAddress
) where

import qualified Language.C.Inline as C

import Data.Word
import Myo.Foreign.Types
import Myo.Foreign.String
import Myo.Foreign.Hub
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

C.context myoCtx
C.include "libmyo.h"
C.include "<string.h>"
C.include "wrapper.h"

-------------------------------------------------------------------------------
errorKind :: ErrorDetails -> IO Result
errorKind ed = withForeignPtr ed $ \ed' -> do
  alloca $ \resPtr -> do
    [C.block| void {
      libmyo_result_t r = libmyo_error_kind($(libmyo_error_details_t ed'));
      memmove($(libmyo_result_t* resPtr)
             , &r
             , sizeof(libmyo_result_t)
             );
      }
    |]
    peek resPtr

-------------------------------------------------------------------------------
-- | Free the resources allocated by the ErrorDetails object.
foreign import ccall "wrapper.h &myo_error_details_free"
  freeErrorDetails :: FunPtr (Ptr ErrorDetails_t -> IO ())

-------------------------------------------------------------------------------
errorCString :: ErrorDetails -> IO CString
errorCString ed = withForeignPtr ed $ \ed' -> do
 [C.exp| const char* { libmyo_error_cstring($(libmyo_error_details_t ed')) }|]

-------------------------------------------------------------------------------
--  Set the locking policy for Myos connected to the hub.
--  @returns libmyo_success if the locking policy is successfully set, otherwise
--  - libmyo_error_invalid_argument if \a hub is NULL
--  - libmyo_error if \a hub is not a valid hub
-- libmyo_result_t libmyo_set_locking_policy(libmyo_hub_t hub, libmyo_locking_policy_t locking_policy,
--                                           libmyo_error_details_t* out_error);


-- Retrieve the MAC address of a Myo.
-- The MAC address is unique to the physical Myo, and is a 48-bit number.
-- uint64_t libmyo_get_mac_address(libmyo_myo_t myo);
getMacAddress :: MyoDevice -> IO Word64
getMacAddress md = withForeignPtr md $ \myo -> do
  [C.exp| uint64_t { libmyo_get_mac_address($(libmyo_myo_t myo))} |]
