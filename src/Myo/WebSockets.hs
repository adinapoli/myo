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
        -> ClientApp a
        -> IO a
connect apiVr aId host port app = do
  runClient host port ("/myo/" <> show apiVr <> "?appid=" <> aId) app

--------------------------------------------------------------------------------
sendCommand :: Connection -> Command -> IO ()
sendCommand conn cmd = do
  sendBinaryData conn (JSON.encode $ JSON.Array (V.fromList ["command", JSON.toJSON cmd]))
