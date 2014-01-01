{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pubnub
import Network.Pubnub.Types
import Data.Aeson
import Control.Concurrent
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  let pn = defaultPN{channels=["hello_world"], sub_key="demo", pub_key="demo"}
  _ <- subscribe pn Nothing output
  _ <- threadDelay 1000000
  hello <- publish pn "hello_world" ("hello" :: L.ByteString)
  print hello
  hello2 <- history pn "hello_world" [ Reverse True
                       , Count 2] :: IO (Maybe (History Value))
  print hello2
  return ()

output :: Maybe Value -> IO ()
output = print
