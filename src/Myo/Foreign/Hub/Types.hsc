{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo.Foreign.Hub.Types where

import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as C
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

#include "libmyo.h"
C.include "libmyo.h"
C.include "<string.h>"

data Hub_t
type MyoHub = ForeignPtr Hub_t

instance Storable Hub_t where
  sizeOf _ = (#size libmyo_hub_t)
  alignment _ = alignment (undefined :: Ptr Hub_t)
  peek _   = error "Hub_t.peek: Absurd"
  poke _ _ = error "Hub_t.poke: Absurd"

hubCtx :: Context
hubCtx = mempty { ctxTypesTable = hubTable }

hubTable :: Map.Map C.TypeSpecifier TH.TypeQ
hubTable = Map.fromList
   [ (C.TypeName "libmyo_hub_t", [t| Ptr Hub_t |])
   ]
