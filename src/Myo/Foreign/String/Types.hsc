{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo.Foreign.String.Types (
    MyoString_t
  , MyoString
  , stringCtx
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

#include "libmyo.h"
C.include "libmyo.h"

data MyoString_t
type MyoString = ForeignPtr MyoString_t

instance Storable MyoString_t where
  sizeOf _ = (#size libmyo_string_t)
  alignment _ = alignment (undefined :: Ptr MyoString)
  peek _   = error "MyoString_t.peek: Absurd"
  poke _ _ = error "MyoString_t.poke: Absurd"

stringCtx :: Context
stringCtx = mempty { ctxTypesTable = typesTable }

typesTable :: Map.Map C.TypeSpecifier TH.TypeQ
typesTable = Map.fromList
   [ (C.TypeName "libmyo_string_t", [t| Ptr MyoString_t |]) ]
