{-# LANGUAGE OverloadedStrings #-}
module Myo.WebSockets (
    module Myo.WebSockets.Types
  , ApplicationID
  , APIVersion(..)
  , connect
  , sendCommand
  ) where

import Network.WebSockets
import Data.Monoid
import Control.Monad
import Control.Concurrent
import qualified Data.Aeson as JSON
import qualified Data.Vector as V
import Myo.WebSockets.Types

data APIVersion = V3

instance Show APIVersion where
  show V3 = "3"

type ApplicationID = String

connect :: APIVersion
        -> ApplicationID
        -- ^ The Myo ApplicationID
        -> String
        -- ^ Host
        -> Int
        -- ^ Port
        -> IO (Chan Frame)
connect apiVr aId host port = do
 ch <- newChan
 void $ forkIO $ runClient host port ("/myo/" <> show apiVr <> "?appid=" <> aId) $ \conn ->
   forever $ do
     newData <- receiveData conn
     let msg = JSON.eitherDecode' newData
     case msg of
       Left e   -> do
         putStrLn e
         print newData
       Right r -> writeChan ch r
 return ch

--------------------------------------------------------------------------------
sendCommand :: Connection -> Command -> IO ()
sendCommand conn cmd = do
  sendBinaryData conn (JSON.encode $ JSON.Array (V.fromList ["command", JSON.toJSON cmd]))
