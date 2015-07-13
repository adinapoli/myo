{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Myo.Foreign.Types where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Data.Monoid

import           Language.C.Inline.Context
import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as C
import qualified Data.Map.Strict as Map

#include "libmyo.h"

newtype MyoString = MStr (Ptr CString)

myoCtx :: Context
myoCtx = baseCtx <> funCtx <> vecCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = myoTypesTable
      }

myoTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
myoTypesTable = Map.fromList [
   (C.TypeName "libmyo_string_t", [t| MyoString |])
  ]
