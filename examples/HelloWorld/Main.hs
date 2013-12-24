{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pubnub

main :: IO ()
main = do
  let pn = defaultPN{channel="hello_world", sub_key="demo", pub_key="demo"}
  hello <- subscribe pn
  putStrLn (show hello)
  hello2 <- publish pn "hello"
  putStrLn (show hello2)
