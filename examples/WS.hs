{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Aeson as JSON
import Network.WebSockets
import Myo.WebSockets
import Control.Monad
import Lens.Family2

main :: IO ()
main = runClient "localhost" 10138 "/myo/3?appid=com.example.appid" myoWS

myoWS :: Connection -> IO ()
myoWS conn = forever $ do
  newData <- receiveData conn
  let (msg :: Either String MyoFrame) = JSON.eitherDecode' newData
  case msg of
    Left e   -> putStrLn e
    Right (Event my) -> case my ^. mye_type of
      EVT_Paired -> putStrLn "MYO PAIRED!"
      EVT_Connected -> putStrLn "MYO CONNECTED!"
      EVT_Pose -> case my ^. mye_pose of
        Nothing -> putStrLn "GOT A POSE!"
        Just  p -> putStrLn $ "GOT POSE: " ++ show p
      EVT_Arm_Synced -> putStrLn "MYO ARM SYNCED"
      EVT_Arm_Unsynced -> putStrLn "MYO ARM UNSYNCED"
      _ -> return ()
