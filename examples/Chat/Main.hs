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
import qualified Data.Text.IO as I
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

data Msg = Msg { username :: T.Text
               , msg      :: T.Text }
           deriving (Show, Generic)

instance ToJSON Msg

instance FromJSON Msg

type ClientName = T.Text

data Client = Client { clientName :: ClientName
                     , pn         :: PN }

main :: IO ()
main = do
  putStrLn "Enter Username: "
  username <- I.getLine
  runClient $ newClient username

newClient :: ClientName -> Client
newClient name = Client { clientName = name
                        , pn = defaultPN { channels = ["testchathaskell2"]
                                         , sub_key  = "demo"
                                         , pub_key  = "demo" }}

runClient :: Client -> IO ()
runClient Client{..} = do
  a <- presenceRun
  b <- receiver
  withAsync (cli a b) $ \c -> do
    _ <- waitAnyCancel [a, b, c]
    return ()
  where
    presenceRun =
      presence pn (encodeUtf8 clientName) outputPresence

    cli a b = forever $ do
      msg <- I.getLine
      case msg of
        "/leave" -> do
          leave pn (head $ channels pn) (encodeUtf8 clientName)
          unsubscribe a
          unsubscribe b
          mzero
        _ ->
          publish pn (head $ channels pn) Msg { username=clientName
                                              , msg=msg }

    receiver =
      subscribe pn (Just (encodeUtf8 clientName)) output

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
output (Just m) =
  I.putStrLn $ T.concat ["<", username m, "> : ", msg m]
output Nothing = return ()

encodeMsg :: Msg -> L.ByteString
encodeMsg = encode

decodeMsg :: L.ByteString -> Maybe [Msg]
decodeMsg = decode
