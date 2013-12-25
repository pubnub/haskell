{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pubnub
import Data.Aeson
import Control.Concurrent
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  let pn = defaultPN{channel="hello_world", sub_key="demo", pub_key="demo"}
  _ <- forkIO $ subscribe pn (output)
  _ <- threadDelay 1000000
  hello <- publish pn ("hello" :: L.ByteString)
  putStrLn (show hello)
  return ()

output :: Maybe Value -> IO ()
output = (putStrLn . show)
