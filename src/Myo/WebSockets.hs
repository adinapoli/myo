{-# LANGUAGE OverloadedStrings #-}
module Myo.WebSockets (
    module Myo.WebSockets.Types
  , ApplicationID
  , APIVersion(..)
  , connect
  ) where

import Network.WebSockets
import Data.Monoid
import Control.Monad
import Control.Concurrent
import qualified Data.Aeson as JSON
import Myo.WebSockets.Types
import qualified Data.Text as T

data APIVersion = V3

type ApplicationID = T.Text

renderAPIVersion :: APIVersion -> T.Text
renderAPIVersion V3 = "3"

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
 void $ forkIO $ runClient host port (T.unpack $ "/myo/" <> renderAPIVersion apiVr <> "?appid=" <> aId) $ \conn ->
   forever $ do
     newData <- receiveData conn
     let msg = JSON.eitherDecode' newData
     case msg of
       Left e   -> do
         putStrLn e
         print newData
       Right r -> writeChan ch r
 return ch
