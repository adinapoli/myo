
module Myo.Foreign.Result where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "libmyo.h"

data Result =
  	Success
  | Error
  | InvalidArgument
	| RuntimeError
	deriving (Show, Eq, Ord)

type ErrorDetails = Ptr ()

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
