{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Myo.WebSockets
import Control.Concurrent
import Lens.Family2

main :: IO ()
main = do
  evtChan <- connect V3 "com.example.appid" "localhost" 10138
  go evtChan
  where
    go evtChan = do
      e <- readChan evtChan
      case e of
        Evt my -> case my ^. mye_type of
            EVT_Paired -> putStrLn "MYO PAIRED!"
            EVT_Connected -> putStrLn "MYO CONNECTED!"
            EVT_Pose -> case my ^. mye_pose of
                Nothing -> putStrLn "GOT A POSE!"
                Just  p -> putStrLn $ "GOT POSE: " ++ show p
            EVT_Arm_Synced -> putStrLn "MYO ARM SYNCED"
            EVT_Arm_Unsynced -> putStrLn "MYO ARM UNSYNCED"
            _ -> return ()
        Cmd c -> print c
      go evtChan
