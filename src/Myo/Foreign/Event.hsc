
module Myo.Foreign.Event where

import Foreign.Storable
import Foreign.Ptr

#include "libmyo.h"

data Event =
      Paired           -- ^ Successfully paired with a Myo.
    | Unpaired         -- ^ Successfully unpaired from a Myo.
    | Connected        -- ^ A Myo has successfully connected.
    | Disconnected     -- ^ A Myo has been disconnected.
    | ArmSynced        -- ^ A Myo has recognized that the sync gesture has been successfully performed.
    | ArmUnsynced      -- ^ A Myo has been moved or removed from the arm.
    | Orientation      -- ^ Orientation data has been received.
    | Pose             -- ^ A change in pose has been detected. @see libmyo_pose_t.
    | Rssi             -- ^ An RSSI value has been received.
    | Unlocked         -- ^ A Myo has become unlocked.
    | Locked           -- ^ A Myo has become locked.
    | Emg              -- ^ EMG data has been received.
    | BatteryLevel     -- ^ A battery level value has been received.
    | WarmupCompleted  -- ^ The warmup period has completed.
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Storable Event where
  sizeOf _ = (#size libmyo_event_type_t)
  alignment _ = alignment (undefined :: Ptr Event)
  peek ptr = do
    v <- peekByteOff ptr 0
    return $ toEnum v
  poke p v = let e = fromEnum v in pokeByteOff p e e
