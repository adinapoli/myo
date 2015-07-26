{-# LANGUAGE OverloadedStrings #-}
module Myo.WebSockets where

--import Network.WebSockets
import qualified Data.Text as T

data APIVersion = V3

type ApplicationID = T.Text

renderAPIVersion :: APIVersion -> T.Text
renderAPIVersion V3 = "3"

{-
connect :: APIVersion
        -> ApplicationID
        -- ^ The Myo ApplicationID
        -> String
        -- ^ Host
        -> Int
        -- ^ Port
        -> IO a
connect apiVr aId host port = do
 runClient host port (T.unpack $ "/myo/" <> renderAPIVersion <> "?appid=" <> aId)
 127.0.0.1:10138/myo/?appid=com.example.appid
-}
