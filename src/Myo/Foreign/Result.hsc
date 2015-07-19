module Myo.Foreign.Result where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

#include "libmyo.h"

data Result =
    Success
  | Error
  | InvalidArgument
  | RuntimeError
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
    return $ case (v :: CInt) of
      0 -> Success
      1 -> Error
      2 -> InvalidArgument
      _ -> RuntimeError
  poke p v = case v of
    Success  -> pokeByteOff p 0 (0 :: Int)
    Error -> pokeByteOff p 1 (1 :: Int)
    InvalidArgument -> pokeByteOff p 2 (2 :: Int)
    RuntimeError  -> pokeByteOff p 3 (3 :: Int)
