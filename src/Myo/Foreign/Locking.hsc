module Myo.Foreign.Locking where

import Foreign.C.Types
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
    return $ case (v :: CInt) of
      0 -> None
      _ -> Standard
  poke p v = case v of
    None     -> pokeByteOff p 0 (0 :: Int)
    Standard -> pokeByteOff p 1 (1 :: Int)
