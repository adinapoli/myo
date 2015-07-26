{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Myo.WebSockets.Types where

import Data.Aeson.TH
import Data.Int
import Data.Scientific
import Data.Aeson.Types
import Data.Char
import Control.Monad
import Lens.Family2.TH
import qualified Data.Vector as V
import qualified Data.Text as T

-------------------------------------------------------------------------------
data MyoEventType =
    EVT_Paired
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
type MyoID = Integer

-------------------------------------------------------------------------------
data MyoVersion = MyoVersion {
    _myv_major    :: !Integer
  , _myv_minor    :: !Integer
  , _myv_patch    :: !Integer
  , _myv_hardware :: !Integer
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- It's an 8 bit integer
data EMG  = EMG Int8 deriving (Show, Eq)

-------------------------------------------------------------------------------
data MyoPose =
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
data MyoArm = Arm_Left | Arm_Right deriving (Show, Eq)

-------------------------------------------------------------------------------
data MyoDirection = Toward_wrist | Toward_elbow deriving (Show, Eq)

-------------------------------------------------------------------------------
data MyoEvent = MyoEvent {
    _mye_type :: !MyoEventType
  , _mye_timestamp :: !T.Text
  , _mye_myo :: !MyoID
  , _mye_arm :: !(Maybe MyoArm)
  , _mye_x_direction :: !(Maybe MyoDirection)
  , _mye_version :: !(Maybe MyoVersion)
  , _mye_rssi :: !(Maybe Int)
  , _mye_pose :: !(Maybe MyoPose)
  , _mye_emg :: !(Maybe EMG)
  , _mye_orientation :: !(Maybe Orientation)
  , _mye_accelerometer :: !(Maybe Accelerometer)
  , _mye_gyroscope :: !(Maybe Gyroscope)
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
instance FromJSON MyoVersion where
 parseJSON (Array v) = do
  let lst = V.toList v
  case liftM2 (,) (Just $ length lst) (mapM toNumber lst) of
   Just (4, x) -> case mapM floatingOrInteger x of
      Right [ma, mi, pa, ha] -> return $ MyoVersion ma mi pa ha
      _ -> mzero
   _ -> mzero
 parseJSON v = typeMismatch "MyoVersion: Expecting an Array like [major, minor, patch, hardware]" v


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
deriveFromJSON defaultOptions { fieldLabelModifier = drop 5 } ''MyoEvent
deriveFromJSON defaultOptions { fieldLabelModifier = drop 5 } ''Orientation
deriveFromJSON defaultOptions { constructorTagModifier = map toLower . drop 4 } ''MyoEventType
deriveFromJSON defaultOptions { constructorTagModifier = map toLower } ''MyoPose
deriveFromJSON defaultOptions { constructorTagModifier = map toLower } ''MyoDirection
deriveFromJSON defaultOptions { constructorTagModifier = map toLower . drop 4 } ''MyoArm

-------------------------------------------------------------------------------
-- Lenses
makeLenses ''MyoEvent
makeLenses ''MyoVersion
