{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Myo.Foreign.String where

import Foreign.Ptr
import Foreign.C

import Myo.Foreign.Types

import qualified Language.C.Inline as C

C.context myoCtx
C.include "libmyo.h"
C.include "wrapper.h"

fromMyoString :: Ptr MyoString -> IO CString
fromMyoString sPtr = [C.exp| const char* { libmyo_string_c_str($(libmyo_string_t *sPtr)) } |]

-- // Free the resources allocated by the string object.
-- LIBMYO_EXPORT
-- void libmyo_string_free(libmyo_string_t);
