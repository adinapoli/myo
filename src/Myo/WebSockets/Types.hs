{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Myo.WebSockets.Types (
    EventType(..)
  , MyoID
  , Version(..)
  , EMG(..)
  , Pose(..)
  , Orientation(..)
  , Accelerometer(..)
  , Gyroscope(..)
  , Arm(..)
  , Direction(..)
  , Frame(..)
  , Event(..)
  , Command
  , GivenCmd(..)
  , CmdData(..)

  -- * Lenses
  , mye_type
  , mye_timestamp
  , mye_myo
  , mye_arm
  , mye_x_direction
  , mye_version
  , mye_warmup_result
  , mye_rssi
  , mye_pose
  , mye_emg
  , mye_orientation
  , mye_accelerometer
  , mye_gyroscope
  , myv_major
  , myv_minor
  , myv_patch
  , myv_hardware

  -- * Smart constructors
  , newCommand
  ) where

import Data.Aeson.TH
import Data.Int
import Data.Scientific
import Data.Aeson.Types hiding (Result)
import Data.Char
import Control.Monad
import Control.Applicative
import Lens.Family2.TH
import qualified Data.Vector as V
import qualified Data.Text as T

-------------------------------------------------------------------------------
data EventType =
    EVT_Paired
  | EVT_Battery_Level
  | EVT_Locked
  | EVT_Unlocked
  | EVT_Warmup_Completed
  | EVT_Connected
  | EVT_Disconnected
  | EVT_Arm_Synced
  | EVT_Arm_Unsynced
  | EVT_Orientation
  | EVT_Pose
  | EVT_RSSI
  | EVT_EMG
  deriving (Show, Eq)

-------------------------------------------------------------------------------
newtype MyoID = MyoID Integer deriving (Show, Eq)

-------------------------------------------------------------------------------
data Version = Version {
    _myv_major    :: !Integer
  , _myv_minor    :: !Integer
  , _myv_patch    :: !Integer
  , _myv_hardware :: !Integer
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- It's an 8 bit integer
data EMG  = EMG Int8 deriving (Show, Eq)

-------------------------------------------------------------------------------
data Pose =
     Rest
   | Fist
   | Wave_In
   | Wave_Out
   | Fingers_Spread
   | Double_Tap
   | Unknown
   deriving (Show, Eq)

data Orientation = Orientation {
     _ori_x :: !Double
   , _ori_y :: !Double
   , _ori_z :: !Double
   , _ori_w :: !Double
   } deriving (Show, Eq)

data Accelerometer = Accelerometer {
     _acc_x :: !Double
   , _acc_y :: !Double
   , _acc_z :: !Double
   } deriving (Show, Eq)

data Gyroscope = Gyroscope {
     _gyr_x :: !Double
   , _gyr_y :: !Double
   , _gyr_z :: !Double
   } deriving (Show, Eq)

-------------------------------------------------------------------------------
data Arm = Arm_Left | Arm_Right deriving (Show, Eq)

-------------------------------------------------------------------------------
data Direction = Toward_wrist | Toward_elbow deriving (Show, Eq)

-------------------------------------------------------------------------------
data Frame = Evt Event
           | Cmd Command
           deriving (Show, Eq)

instance FromJSON Frame where
 parseJSON (Array v) = case V.toList v of
   [String "event", o@(Object _)] -> Evt <$> parseJSON o
   [String "command", o@(Object _)] -> Cmd <$> parseJSON o
   _ -> mzero
 parseJSON v = typeMismatch "Frame: Expecting an Array of frames." v


-------------------------------------------------------------------------------
-- TODO: Break down this `Event` type into a mandatory section (type, timestamp, myo)
-- and a payload specific field, so that we do not have all this proliferation of
-- Maybe. The `FromJSON` instance will need to be written manually, but it's not
-- too bad.
data Event = Event {
    _mye_type :: !EventType
  , _mye_timestamp :: !T.Text
  , _mye_myo :: !MyoID
  , _mye_arm :: !(Maybe Arm)
  , _mye_x_direction :: !(Maybe Direction)
  , _mye_version :: !(Maybe Version)
  , _mye_warmup_result :: !(Maybe Result)
  , _mye_rssi :: !(Maybe Int)
  , _mye_pose :: !(Maybe Pose)
  , _mye_emg :: !(Maybe EMG)
  , _mye_orientation :: !(Maybe Orientation)
  , _mye_accelerometer :: !(Maybe Accelerometer)
  , _mye_gyroscope :: !(Maybe Gyroscope)
  } deriving (Show, Eq)


-------------------------------------------------------------------------------
data Result = Success | Fail deriving (Show, Eq)

-------------------------------------------------------------------------------
data CommandType =
    COM_vibrate
  | COM_request_rssi
  | COM_set_stream_emg
  | COM_set_locking_policy
  | COM_unlock
  | COM_lock
  | COM_notify_user_action
  deriving (Show, Eq)

-------------------------------------------------------------------------------
data CommandData =
    COD_Short
  | COD_Medium
  | COD_Long
  | COD_Enabled
  | COD_Disabled
  deriving (Show, Eq)

-------------------------------------------------------------------------------
data Command = Command {
    _myc_command :: !CommandType
  , _myc_myo :: !MyoID
  , _myc_type :: CommandData
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Find a type in a type-level list.
type family Find x xs where
  Find x '[] = False
  Find x (x ': xs) = True
  Find x (y ': xs) = Find x xs

data GivenCmd :: CommandType -> [CommandData] -> * where
  Vibrate        :: GivenCmd 'COM_vibrate '[COD_Short, COD_Medium, COD_Long]
  Set_Stream_EMG :: GivenCmd 'COM_vibrate '[COD_Enabled, COD_Disabled]

data CmdData :: CommandData -> * where
  Short    :: CmdData 'COD_Short
  Medium   :: CmdData 'COD_Medium
  Long     :: CmdData 'COD_Long
  Enabled  :: CmdData 'COD_Enabled
  Disabled :: CmdData 'COD_Disabled

-- NOTE: Can these be inferred/generated automatically? (TH?)
toCommandType :: GivenCmd k a -> CommandType
toCommandType Vibrate = COM_vibrate
toCommandType Set_Stream_EMG = COM_set_stream_emg

toCommandData :: CmdData k -> CommandData
toCommandData Short    = COD_Short
toCommandData Medium   = COD_Medium
toCommandData Long     = COD_Long
toCommandData Enabled  = COD_Enabled
toCommandData Disabled = COD_Disabled

-- newCommand 0 Vibrate COD_Short :: Command
newCommand :: (Find k acceptedCmds ~ True)
           => MyoID
           -> GivenCmd cmdType acceptedCmds
           -> CmdData k
           -> Command
newCommand mid ct s = Command (toCommandType ct) mid (toCommandData s)

-------------------------------------------------------------------------------
instance FromJSON Version where
 parseJSON (Array v) = do
  let lst = V.toList v
  case liftM2 (,) (Just $ length lst) (mapM toNumber lst) of
   Just (4, x) -> case mapM floatingOrInteger x of
      Right [ma, mi, pa, ha] -> return $ Version ma mi pa ha
      _ -> mzero
   _ -> mzero
 parseJSON v = typeMismatch "Version: Expecting an Array like [major, minor, patch, hardware]" v


toNumber :: Value -> Maybe Scientific
toNumber (Number v) = Just v
toNumber _ = Nothing

-- TODO: Create an Int8 in a better way than this one!
instance FromJSON EMG where
 parseJSON (Array v) = do
  let lst = V.toList v
  case liftM2 (,) (Just $ length lst) (mapM toNumber lst) of
   Just (8, x) -> case mapM floatingOrInteger x of
      Right res -> return . EMG . read $ concatMap show res
      _ -> mzero
   _ -> mzero
 parseJSON v = typeMismatch "EMG: Expecting an Array of size 8." v

instance FromJSON Gyroscope where
 parseJSON (Array v) = case V.toList v of
   [Number x, Number y, Number z] -> return $ Gyroscope (toRealFloat x) (toRealFloat y) (toRealFloat z)
   _ -> mzero
 parseJSON v = typeMismatch "Gyroscope: Expecting an Array of Double like [x,y,z]" v

instance FromJSON Accelerometer where
 parseJSON (Array v) = case V.toList v of
   [Number x, Number y, Number z] -> return $ Accelerometer (toRealFloat x) (toRealFloat y) (toRealFloat z)
   _ -> mzero
 parseJSON v = typeMismatch "Accelerometer: Expecting an Array of Double like [x,y,z]" v

-------------------------------------------------------------------------------
-- JSON
deriveJSON defaultOptions ''MyoID
deriveFromJSON defaultOptions { fieldLabelModifier = drop 5 } ''Event
deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''Command
deriveJSON defaultOptions { constructorTagModifier = map toLower . drop 4 } ''CommandType
deriveJSON defaultOptions { constructorTagModifier = map toLower . drop 4 } ''CommandData
deriveFromJSON defaultOptions { constructorTagModifier = map toLower } ''Result
deriveFromJSON defaultOptions { fieldLabelModifier = drop 5 } ''Orientation
deriveFromJSON defaultOptions { constructorTagModifier = map toLower . drop 4 } ''EventType
deriveFromJSON defaultOptions { constructorTagModifier = map toLower } ''Pose
deriveFromJSON defaultOptions { constructorTagModifier = map toLower } ''Direction
deriveFromJSON defaultOptions { constructorTagModifier = map toLower . drop 4 } ''Arm

-------------------------------------------------------------------------------
-- Lenses
makeLenses ''Event
makeLenses ''Version
