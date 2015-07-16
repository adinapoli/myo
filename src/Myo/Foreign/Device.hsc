module Myo.Foreign.Device where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

#include "libmyo.h"

type MyoDevice = Ptr ()

data Vibration =
  VibrationShort
	| VibrationMedium
	| VibrationLong

instance Storable Vibration where
	sizeOf _ = (#size libmyo_vibration_type_t)
	alignment _ = alignment (undefined :: Ptr Vibration)
	peek ptr = do
	  v <- peekByteOff ptr 0
	  return $ case (v :: CInt) of
	    0 -> VibrationShort
	    1 -> VibrationMedium
	    _ -> VibrationLong
	poke p v = case v of
	  VibrationShort  -> pokeByteOff p 0 (0 :: Int)
	  VibrationMedium -> pokeByteOff p 1 (1 :: Int)
	  VibrationLong   -> pokeByteOff p 2  (2 :: Int)
