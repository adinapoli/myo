{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo (
    module Myo.Foreign.Types
  , module Myo.Foreign.Hub
  , module Myo.Foreign.String
  , errorKind
  , freeErrorDetails
  , errorCString
) where

import qualified Language.C.Inline as C

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
