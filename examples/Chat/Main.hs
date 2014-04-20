{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Pubnub
import Network.Pubnub.Types

import GHC.Generics
import Data.Aeson
import Data.Maybe

import Control.Concurrent.Async
import Control.Monad
import qualified Data.Text.IO as I
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

data Msg = Msg { username :: T.Text
               , msg      :: T.Text }
           deriving (Show, Generic)

instance ToJSON Msg

instance FromJSON Msg

main :: IO ()
main = do
  putStrLn "Enter Username: "
  username <- I.getLine
  runClient $ newClient username True

newClient :: T.Text -> Bool -> PN
newClient name encrypt
  | encrypt = either (error . show) (\x -> x{ uuid_key = (Just name) }) encKey
  | otherwise       = newPN
  where
    encKey = setEncryptionKey newPN "enigma"

    newPN  = defaultPN { uuid_key = (Just name)
                       , channels = ["testchathaskell2"]
                       , sub_key  = "demo"
                       , pub_key  = "demo"
                       , ssl      = False }

runClient :: PN -> IO ()
runClient pn = do
  a <- receiver
  withAsync (cli a) $ \b -> do
    _ <- waitAnyCancel [a, b]
    return ()
  where          
    cli a = forever $ do
      msg <- I.getLine
      case msg of
        "/leave" -> do
          leave pn (head $ channels pn) (fromJust $ uuid_key pn)
          unsubscribe a
          mzero
        _ ->
          publish pn (head $ channels pn) Msg { username=(fromJust $ uuid_key pn)
                                              , msg=msg }

    receiver =
      subscribe pn defaultSubscribeOptions{ onPresence = Just outputPresence
                                          , onMsg = output
                                          , onConnect = putStrLn "Connected..." }

outputPresence :: Presence -> IO ()
outputPresence Presence{..} = do
  I.putStr "** "
  I.putStr uuid
  case action of
    Join ->
      putStrLn " has joined channel"
    Leave ->
      putStrLn " has left channel"
    Timeout ->
      putStrLn " has dropped from channel"

output :: Maybe Msg -> IO ()
output (Just m) =
  I.putStrLn $ T.concat ["<", username m, "> : ", msg m]
output Nothing = return ()

encodeMsg :: Msg -> L.ByteString
encodeMsg = encode

decodeMsg :: L.ByteString -> Maybe [Msg]
decodeMsg = decode
