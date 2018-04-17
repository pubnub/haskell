{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.Text            as T
import           Network.Pubnub
import           Network.Pubnub.Types

main :: IO ()
main = do
  pn' <- defaultPN
  let pn = pn'{channels=["hello_world"], sub_key="demo", pub_key="demo"}
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
