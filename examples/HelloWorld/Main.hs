{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pubnub
import Network.Pubnub.Types
import Data.Aeson
import Control.Concurrent
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  let pn = defaultPN{channel="hello_world", sub_key="demo", pub_key="demo"}
  _ <- subscribe pn Nothing output
  _ <- threadDelay 1000000
  hello <- publish pn ("hello" :: L.ByteString)
  print hello
  hello2 <- history pn [ Reverse True
                       , Count 2] :: IO (Maybe (History Value))
  print hello2
  return ()

output :: Maybe Value -> IO ()
output = print
