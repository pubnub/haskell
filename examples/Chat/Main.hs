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

data Client = Client { clientName :: ClientName
                     , pn         :: PN }

main :: IO ()
main = do
  putStrLn "Enter Username: "
  username <- B.getLine
  runClient $ newClient username

newClient :: ClientName -> Client
newClient name = Client { clientName = name
                        , pn = defaultPN { channel="testchathaskell"
                                         , sub_key="demo"
                                         , pub_key="demo" }}

runClient :: Client -> IO ()
runClient Client{..} =
  withAsync presenceRun $ \a ->
    withAsync cli $ \b ->
      withAsync receiver $ \c -> do
        _ <- waitAnyCancel [a, b, c]
        return ()
  where
    presenceRun = do
      presence pn clientName (outputPresence)

    cli = forever $ do
      msg <- B.getLine
      case msg of
        "/leave" -> do
          leave pn clientName
          mzero
        _ ->
          publish pn (Msg { username=clientName
                          , msg=msg })

    receiver = do
      subscribe pn (Just clientName) (output)

outputPresence :: Maybe Presence -> IO ()
outputPresence (Just Presence{..}) = do
  B.putStr "** "
  B.putStr uuid
  case action of
    Join ->
      putStrLn " has joined channel"
    Leave ->
      putStrLn " has left channel"
    Timeout ->
      putStrLn " has dropped from channel"
outputPresence _ = return ()

output :: Maybe Msg -> IO ()
output (Just m) = B.putStrLn $ B.concat ["<", (username m), "> : ", (msg m)]
output Nothing = return ()

encodeMsg :: Msg -> L.ByteString
encodeMsg = encode

decodeMsg :: L.ByteString -> Maybe [Msg]
decodeMsg = decode
