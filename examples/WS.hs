{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Aeson as JSON
import Network.WebSockets
import Myo.WebSockets
import Control.Monad

main :: IO ()
main = runClient "localhost" 10138 "/myo/3?appid=com.example.appid" myoWS

myoWS :: Connection -> IO ()
myoWS conn = forever $ do
  newData <- receiveData conn
  let (msg :: Either String MyoFrame) = JSON.eitherDecode' newData
  case msg of
    Left e   -> putStrLn e
    Right my -> print my
