module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)
--import Test.Tasty.HUnit

--import Network.Pubnub

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ ]
