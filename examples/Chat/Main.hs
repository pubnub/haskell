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
  runClient $ newClient username True

newClient :: ClientName -> Bool -> Client
newClient name encrypt
  | encrypt = either (error . show) (\x -> Client { clientName = name
                                                          , pn         = x}) encKey
  | otherwise       = Client { clientName = name
                             , pn         = newPN}
  where
    encKey = setEncryptionKey newPN "enigma"

    newPN  = defaultPN { channels = ["testchathaskell2"]
                       , sub_key  = "demo"
                       , pub_key  = "demo"
                       , ssl      = False }

runClient :: Client -> IO ()
runClient Client{..} = do
  a <- receiver
  withAsync (cli a) $ \b -> do
    _ <- waitAnyCancel [a, b]
    return ()
  where
    cli a = forever $ do
      msg <- I.getLine
      case msg of
        "/leave" -> do
          leave pn (head $ channels pn) clientName
          unsubscribe a
          mzero
        _ ->
          publish pn (head $ channels pn) Msg { username=clientName
                                              , msg=msg }

    receiver =
      subscribe pn defaultSubscribeOptions{ uid = Just clientName

                                          , onPresence = Just outputPresence
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
