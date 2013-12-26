{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Pubnub
       (
         PN(..)
       , defaultPN
       , Timestamp(..)

         -- API function
       , time
       , publish
       , subscribe
       , hereNow
       , presence
       , history
       , leave
       , getUuid
       , unsubscribe
       ) where

import Network.Pubnub.Types

import Data.Aeson
import Network.HTTP.Conduit
import Control.Monad.Trans
import Network.HTTP.Types.URI
import Control.Exception.Lifted (try)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

time :: IO (Maybe Timestamp)
time = do
  req <- buildRequest (defaultPN) ["time", "0"] []
  res <- withManager $ httpLbs req
  return (decode $ responseBody res :: Maybe Timestamp)

subscribe :: (FromJSON b) => PN -> (b -> IO ()) -> IO ()
subscribe pn fn = do
  req <- buildRequest pn [ "subscribe"
                         , (sub_key pn)
                         , (channel pn)
                         , bsFromInteger $ jsonp_callback pn
                         , head . L.toChunks $ encode (time_token pn)] []
  withManager $ \manager -> do
    eres <- try $ httpLbs req manager
    case eres of
      Right r -> do
        case decode $ responseBody r of
          Just (SubscribeResponse (resp, t)) -> do
            _ <- lift $ mapM fn resp
            lift $ subscribe (pn { time_token=t }) fn
          Nothing -> do
            lift $ subscribe pn fn
      Left (ResponseTimeout :: HttpException) -> do
        lift $ subscribe pn fn
      Left _ ->
        return ()

publish :: ToJSON a => PN -> a -> IO (Maybe PublishResponse)
publish pn msg = do
  req <- buildRequest pn ["publish"
                         , (pub_key pn)
                         , (sub_key pn)
                         , (sec_key pn)
                         , (channel pn)
                         , bsFromInteger $ jsonp_callback pn
                         , head . L.toChunks $ encode msg] []
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

hereNow :: PN -> IO (Maybe HereNow)
hereNow pn = do
  req <- buildRequest pn [ "v2"
                         , "presence"
                         , "sub-key"
                         , (sub_key pn)
                         , "channel"
                         , (channel pn)] []
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

presence :: PN -> IO (Maybe Presence)
presence pn = do
  return Nothing

history :: FromJSON b => PN -> HistoryOptions -> IO (Maybe (History b))
history pn options = do
  req <- buildRequest pn [ "v2"
                         , "history"
                         , "sub-key"
                         , (sub_key pn)
                         , "channel"
                         , (channel pn)] (convertHistoryOptions options)
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

leave :: PN -> IO ()
leave pn = do
  return ()

getUuid :: PN -> IO (Maybe UUID)
getUuid pn = do
  return Nothing

unsubscribe :: PN -> IO ()
unsubscribe pn = do
  return ()

buildRequest :: PN -> [B.ByteString] -> SimpleQuery -> IO Request
buildRequest pn elems qs = do
  req <- parseUrl "http://"
  return req { host           = (origin pn)
             , path           = B.intercalate "/" elems
             , method         = "GET"
             , secure         = False
             , port           = 80
             , requestHeaders = [("V", "3.1"), ("User-Agent", "Haskell"), ("Accept", "*/*")]
             , queryString    = renderSimpleQuery True qs }

bsFromInteger :: Integer -> B.ByteString
bsFromInteger = B.pack . show
