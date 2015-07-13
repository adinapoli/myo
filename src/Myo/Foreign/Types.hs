{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo.Foreign.Types where

import Data.Monoid

import           Language.C.Inline.Context
import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as C
import qualified Language.C.Inline as C
import qualified Data.Map.Strict as Map

C.include "libmyo.h"

type MyoString = ()

myoCtx :: Context
myoCtx = baseCtx <> funCtx <> vecCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = stringTypesTable
      }

stringTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
stringTypesTable = Map.fromList
   [ (C.TypeName "libmyo_string_t", [t| MyoString |]) ]
