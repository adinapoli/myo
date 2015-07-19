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
import Foreign.Storable
import System.IO.Unsafe

C.context myoCtx
C.include "libmyo.h"

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
