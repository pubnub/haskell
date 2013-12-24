{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pubnub
import Data.Aeson
import Network.Pubnub.Types
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  let pn = defaultPN{channel="hello_world", sub_key="demo", pub_key="demo"}
  hello <- subscribe pn :: IO (Maybe (SubscribeResponse Value))
  putStrLn (show hello)
  hello2 <- publish pn ("hello" :: L.ByteString)
  putStrLn (show hello2)
