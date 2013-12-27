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
  _ <- subscribe pn Nothing (output)
  _ <- threadDelay 1000000
  hello <- publish pn ("hello" :: L.ByteString)
  putStrLn (show hello)
  hello2 <- history pn [ Start 13880217220206290
                       , End 13880217412341907
                       , Reverse True
                       , Count 2] :: IO (Maybe (History Value))
  putStrLn (show hello2)
  return ()

output :: Maybe Value -> IO ()
output = (putStrLn . show)
