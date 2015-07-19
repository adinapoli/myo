{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo.Foreign.Types (
    module Myo.Foreign.Device
  , module Myo.Foreign.Result
  , module Myo.Foreign.String
  , myoCtx
  , MyoHub
  , ApplicationID
  ) where

import Data.Monoid

import           Language.C.Inline.Context
import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as C
import qualified Language.C.Inline as C
import qualified Data.Map.Strict as Map

import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.C.String
import           Myo.Foreign.Device
import           Myo.Foreign.Result
import           Myo.Foreign.String

#include "libmyo.h"
C.include "libmyo.h"

data Hub_t
type MyoHub = ForeignPtr Hub_t

instance Storable Hub_t where
  sizeOf _ = (#size libmyo_hub_t)
  alignment _ = alignment (undefined :: Ptr Hub_t)
  peek _   = error "Hub_t.peek: Absurd"
  poke _ _ = error "Hub_t.poke: Absurd"

type ApplicationID = CString


myoCtx :: Context
myoCtx = baseCtx <> funCtx <> vecCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = stringTypesTable
      }

stringTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
stringTypesTable = Map.fromList
   [ (C.TypeName "libmyo_string_t", [t| MyoString |])
   , (C.TypeName "libmyo_vibration_type_t", [t| Vibration |])
   , (C.TypeName "libmyo_result_t", [t| Result |])
   , (C.TypeName "libmyo_error_details_t", [t| ErrorDetails_t |])
   , (C.TypeName "libmyo_hub_t", [t| Hub_t |])
   ]
