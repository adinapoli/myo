module Myo.Foreign.Locking where

import Foreign.Ptr
import Foreign.Storable

#include "libmyo.h"

data LockingPolicy = None | Standard
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Storable LockingPolicy where
  sizeOf _ = (#size libmyo_locking_policy_t)
  alignment _ = alignment (undefined :: Ptr LockingPolicy)
  peek ptr = do
    v <- peekByteOff ptr 0
    return $ toEnum v
  poke p v = let e = fromEnum v in pokeByteOff p e e
