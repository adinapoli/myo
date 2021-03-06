{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Myo.WebSockets
import Network.WebSockets
import qualified Data.Aeson as JSON
import Control.Monad
import Data.String.Conv
import Lens.Family2

main :: IO ()
main = connect V3 "com.example.appid" "localhost" 10138 myoApp

myoApp :: Connection -> IO ()
myoApp conn = forever $ do
  newData <- receiveData conn
  let msg = JSON.eitherDecode' newData
  case msg of
    Left e   -> do
      putStrLn e
      print newData
    Right r -> case r of
      Evt my -> case my ^. mye_type of
          EVT_Paired -> putStrLn "MYO PAIRED!"
          EVT_Connected -> putStrLn "MYO CONNECTED!"
          EVT_Pose -> case my ^. mye_pose of
              Nothing -> putStrLn "GOT A POSE!"
              Just  Double_Tap -> do
                putStrLn $ "GOT POSE: " ++ show Double_Tap
                let vCmd = newCommand (my ^. mye_myo) (Set_Locking_Policy LKP_standard)
                putStrLn $ toS $ JSON.encode vCmd
                sendCommand conn vCmd
              Just  p -> putStrLn $ "GOT POSE: " ++ show p
          EVT_Arm_Synced -> do
            putStrLn "MYO ARM SYNCED"
          EVT_Arm_Unsynced -> putStrLn "MYO ARM UNSYNCED"
          _ -> return ()
      Cmd c -> print c
      Ack a -> do
        putStrLn "Acknowledged command!"
        print a
