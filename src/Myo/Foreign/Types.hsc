{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo.Foreign.Types (
    module Myo.Foreign.Device
  , module Myo.Foreign.Result
  , module Myo.Foreign.Locking
  , module Myo.Foreign.Event
  , module Myo.Foreign.Handler
  , myoCtx
  , ApplicationID
  ) where

import Data.Monoid

import           Language.C.Inline.Context
import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as C
import qualified Language.C.Inline as C
import qualified Data.Map.Strict as Map

import           Foreign.Ptr
import           Foreign.C.String
import           Myo.Foreign.Device
import           Myo.Foreign.Result
import           Myo.Foreign.Locking
import           Myo.Foreign.Event
import           Myo.Foreign.Handler

#include "libmyo.h"
C.include "libmyo.h"

type ApplicationID = CString

myoCtx :: Context
myoCtx = baseCtx <> funCtx <> vecCtx <> mempty { ctxTypesTable = typesTable }

typesTable :: Map.Map C.TypeSpecifier TH.TypeQ
typesTable = Map.fromList
   [ (C.TypeName "libmyo_vibration_type_t", [t| Vibration |])
   , (C.TypeName "libmyo_result_t", [t| Result |])
   , (C.TypeName "libmyo_error_details_t", [t| Ptr ErrorDetails_t |])
   , (C.TypeName "libmyo_myo_t", [t| Ptr MyoDevice_t |])
   , (C.TypeName "libmyo_locking_policy_t", [t| LockingPolicy |])
   , (C.TypeName "libmyo_event_type_t", [t| EventType |])
   , (C.TypeName "libmyo_event_t", [t| Ptr Event_t |])
   , (C.TypeName "libmyo_handler_result_t", [t| HandlerResult |])
   , (C.TypeName "libmyo_handler_t", [t| Handler |])
   ]
