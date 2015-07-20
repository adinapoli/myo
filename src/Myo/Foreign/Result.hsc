module Myo.Foreign.Result where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

#include "libmyo.h"

data Result =
    Success
  | Error
  | InvalidArgument
  | RuntimeError
  deriving (Show, Eq, Ord, Bounded, Enum)

data ErrorReport =
  ErrorReport Result ErrorDetails
  deriving (Show, Eq, Ord)

data ErrorDetails_t
type ErrorDetails = ForeignPtr ErrorDetails_t

instance Storable ErrorDetails_t where
  sizeOf _ = (#size libmyo_error_details_t)
  alignment _ = alignment (undefined :: Ptr ErrorDetails_t)
  peek _   = error "ErrorDetails_t.peek: Absurd"
  poke _ _ = error "ErrorDetails_t.poke: Absurd"

instance Storable Result where
  sizeOf _ = (#size libmyo_result_t)
  alignment _ = alignment (undefined :: Ptr Result)
  peek ptr = do
    v <- peekByteOff ptr 0
    return $ toEnum v
  poke p v = let e = fromEnum v in pokeByteOff p e e
