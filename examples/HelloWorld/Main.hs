{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pubnub
import Network.Pubnub.Types
import Data.Aeson
import Control.Concurrent
import qualified Data.Text as T

main :: IO ()
main = do
  let pn = defaultPN{channels=["hello_world"], sub_key="demo", pub_key="demo"}
  _ <- subscribe pn defaultSubscribeOptions{ onMsg = output
                                           , onConnect = putStrLn "Connected..." }
  _ <- threadDelay 1000000
  hello <- publish pn "hello_world" ("hello" :: T.Text)
  print hello
  hello2 <- history pn "hello_world" [ Reverse True
                       , Count 2] :: IO (Maybe (History Value))
  print hello2
  return ()

output :: Maybe Value -> IO ()
output = print
