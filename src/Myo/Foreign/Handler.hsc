module Myo.Foreign.Handler where

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr

import Myo.Foreign.Event

#include "libmyo.h"

data HandlerResult =
      Continue     -- ^ Continue processing events
    | Stop         -- ^ Stop processing events
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Storable HandlerResult where
  sizeOf _ = (#size libmyo_handler_result_t)
  alignment _ = alignment (undefined :: Ptr HandlerResult)
  peek ptr = do
    v <- peekByteOff ptr 0
    return $ toEnum v
  poke p v = let e = fromEnum v in pokeByteOff p e e


type Handler = FunPtr (UserData -> Event -> HandlerResult)

type UserData = ForeignPtr ()
