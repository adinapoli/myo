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
  , CommandData(..)
  , Vibration(..)
  , StreamEMGStatus(..)
  , UnlockMode(..)
  , UserAction(..)
  , LockingPolicy(..)

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
import Data.Monoid
import Data.Scientific
import Data.Aeson.Types hiding (Result)
import Data.Char
import Control.Monad
import Control.Applicative
import Lens.Family2.TH
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

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
           | Ack Acknowledgement
           deriving (Show, Eq)

instance FromJSON Frame where
 parseJSON a@(Array v) = case V.toList v of
   [String "event", o@(Object _)] -> Evt <$> parseJSON o
   [String "acknowledgement", o@(Object _)] -> Ack <$> parseJSON o
   [String "command", o@(Object b)] -> case HM.lookup "result" b of
     Nothing -> Cmd <$> parseJSON o
     Just _  -> Ack <$> parseJSON o
   _ -> typeMismatch "Frame: Unexpected payload in Array." a
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

data Acknowledgement = Acknowledgement {
    _ack_command :: AcknowledgedCommand
  , _ack_result :: Result
  } deriving (Show, Eq)

data AcknowledgedCommand =
    ACC_set_locking_policy
  | ACC_set_stream_emg
  deriving (Show, Eq)

-------------------------------------------------------------------------------
data Result = Success | Fail deriving (Show, Eq)

-------------------------------------------------------------------------------
data CommandData =
    Vibrate Vibration
  | Set_Stream_EMG StreamEMGStatus
  | Unlock UnlockMode
  | Notify_User_Action UserAction
  | Set_Locking_Policy LockingPolicy
  | Request_RSSI
  | Lock
  deriving (Show, Eq)

instance ToJSON CommandData where
  toJSON cd = case cd of
    Vibrate v            -> object ["command" .= String "vibrate", "type" .= toJSON v]
    Set_Stream_EMG v     -> object ["command" .= String "set_stream_emg", "type" .= toJSON v]
    Unlock v             -> object ["command" .= String "unlock", "type" .= toJSON v]
    Notify_User_Action v -> object ["command" .= String "notify_user_action", "type" .= toJSON v]
    Set_Locking_Policy v -> object ["command" .= String "set_locking_policy", "type" .= toJSON v]
    Lock                 -> object ["command" .= String "lock"]
    Request_RSSI         -> object ["command" .= String "request_rssi"]

instance FromJSON CommandData where
  parseJSON (Object o) = do
    (cmd :: T.Text) <- o .: "command"
    typ             <- o .: "type"
    case cmd of
      "vibrate"            -> Vibrate <$> parseJSON typ
      "request_rssi"       -> pure Request_RSSI
      "set_stream_emg"     -> Set_Stream_EMG <$> parseJSON typ
      "set_locking_policy" -> Set_Locking_Policy <$> parseJSON typ
      "unlock"             -> Unlock <$> parseJSON typ
      "lock"               -> pure Lock
      "notify_user_action" -> Notify_User_Action <$> parseJSON typ
      t -> fail $ "FromJSON CommandData: invalid 'command' found: " <> show t
  parseJSON t = typeMismatch ("CommandData, expected Object, found " <> show t) t

-------------------------------------------------------------------------------
data Vibration = VIB_short
               | VIB_medium
               | VIB_long deriving (Show, Eq)

-------------------------------------------------------------------------------
data UserAction = UAC_single deriving (Show, Eq)

-------------------------------------------------------------------------------
data LockingPolicy = LKP_standard
                   | LKP_none deriving (Show, Eq)

-------------------------------------------------------------------------------
data UnlockMode = UMD_timed
                | UMD_hold deriving (Show, Eq)

-------------------------------------------------------------------------------
data StreamEMGStatus = SES_enabled
                     | SES_disabled deriving (Show, Eq)

-------------------------------------------------------------------------------
data Command = Command {
    _myc_myo  :: !MyoID
  , _myc_info :: CommandData
  } deriving (Show, Eq)

instance FromJSON Command where
  parseJSON v@(Object o) = Command <$> o .: "myo" <*> parseJSON v
  parseJSON v = fail $ "FromJSON Command, expected Object, found " <> show v

instance ToJSON Command where
  toJSON (Command mid cd) = case toJSON cd of
    Object b -> object $ ["myo" .= mid] <> HM.toList b
    v -> error $ "Command: toJSON of CommandData failed, found " <> show v

--------------------------------------------------------------------------------
-- | Creates a new `Command`, to be sent to the Myo armband.
newCommand :: MyoID -> CommandData -> Command
newCommand mid cd = Command mid cd

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

-------------------------------------------------------------------------------
toNumber :: Value -> Maybe Scientific
toNumber (Number v) = Just v
toNumber _ = Nothing

-------------------------------------------------------------------------------
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
--
-- JSON instances
--

deriveJSON defaultOptions ''MyoID
deriveFromJSON defaultOptions { fieldLabelModifier = drop 5 } ''Event
deriveFromJSON defaultOptions { constructorTagModifier = map toLower } ''Result
deriveJSON defaultOptions { constructorTagModifier = drop 4 . map toLower } ''Vibration
deriveJSON defaultOptions { constructorTagModifier = drop 4 . map toLower } ''StreamEMGStatus
deriveJSON defaultOptions { constructorTagModifier = drop 4 . map toLower } ''LockingPolicy
deriveJSON defaultOptions { constructorTagModifier = map toLower } ''UserAction
deriveJSON defaultOptions { constructorTagModifier = map toLower } ''UnlockMode
deriveFromJSON defaultOptions { fieldLabelModifier = drop 5 } ''Orientation
deriveFromJSON defaultOptions { constructorTagModifier = map toLower . drop 4 } ''EventType
deriveFromJSON defaultOptions { constructorTagModifier = map toLower } ''Pose
deriveFromJSON defaultOptions { constructorTagModifier = map toLower } ''Direction
deriveFromJSON defaultOptions { constructorTagModifier = map toLower . drop 4 } ''Arm
deriveFromJSON defaultOptions { fieldLabelModifier = drop 5 } ''Acknowledgement
deriveFromJSON defaultOptions { constructorTagModifier = drop 4 } ''AcknowledgedCommand

-------------------------------------------------------------------------------
--
-- Lenses
--

makeLenses ''Event
makeLenses ''Version
