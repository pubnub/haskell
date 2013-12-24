{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Pubnub
import Network.Pubnub.Types

import GHC.Generics
import Data.Aeson

import Control.Concurrent.Async
import Control.Monad

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

data Msg = Msg { username :: B.ByteString
               , msg      :: B.ByteString }
           deriving (Show, Generic)

instance ToJSON Msg

instance FromJSON Msg

type ClientName = B.ByteString

data Client a b = Client { clientName :: ClientName
                         , pn         :: PN a b }

main :: IO ()
main = do
  putStrLn "Enter Username: "
  username <- B.getLine
  runClient $ newClient username

newClient :: ClientName -> Client Msg [Msg]
newClient name = Client { clientName = name
                        , pn = defaultPN { channel="testchathaskell"
                                         , sub_key="demo"
                                         , pub_key="demo"
                                         , decodeJson=decodeMsg
                                         , encodeJson=encodeMsg }}

runClient :: Client Msg [Msg] -> IO ()
runClient Client{..} = do
  _ <- race cli (receiver (Timestamp 0))
  return ()
  where
    cli = forever $ do
      msg <- B.getLine
      publish pn (Msg { username=clientName
                      , msg=msg })

    receiver time_token = do
      msg <- subscribe (pn { time_token=time_token })
      _ <- output msg
      receiver (getTimeToken msg)

getTimeToken :: Maybe (SubscribeResponse [Msg]) -> Timestamp
getTimeToken (Just (SubscribeResponse (_, t))) = t
getTimeToken Nothing = Timestamp 0

output :: Maybe (SubscribeResponse [Msg]) -> IO [()]
output (Just (SubscribeResponse (msgs, _))) = do
  mapM (\x -> B.putStrLn $ B.concat ["<", (username x), "> : ", (msg x)]) msgs
output Nothing = do
  return [()]

encodeMsg :: Msg -> L.ByteString
encodeMsg = encode

decodeMsg :: L.ByteString -> Maybe [Msg]
decodeMsg = decode
