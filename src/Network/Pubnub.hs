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
import Data.UUID.V4
import Network.HTTP.Conduit
import Control.Monad.Trans
import Network.HTTP.Types.URI
import Control.Applicative ((<$>))
import Control.Exception.Lifted (try)

import qualified Data.UUID as U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

time :: IO (Maybe Timestamp)
time = do
  req <- buildRequest (defaultPN) ["time", "0"] []
  res <- withManager $ httpLbs req
  return (decode $ responseBody res :: Maybe Timestamp)

subscribe :: (FromJSON b) => PN -> Maybe UUID -> (b -> IO ()) -> IO ()
subscribe pn uid fn = do
  req <- buildRequest pn [ "subscribe"
                         , (sub_key pn)
                         , (channel pn)
                         , bsFromInteger $ jsonp_callback pn
                         , head . L.toChunks $ encode (time_token pn)] (case uid of
                                                                           Just u -> [("uuid", u)]
                                                                           Nothing -> [])
  withManager $ \manager -> do
    eres <- try $ httpLbs req manager
    case eres of
      Right r -> do
        case decode $ responseBody r of
          Just (SubscribeResponse (resp, t)) -> do
            _ <- lift $ mapM fn resp
            lift $ subscribe (pn { time_token=t }) uid fn
          Nothing -> do
            lift $ subscribe pn uid fn
      Left (ResponseTimeout :: HttpException) -> do
        lift $ subscribe pn uid fn
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

presence :: (FromJSON b) => PN -> UUID -> (b -> IO ()) -> IO ()
presence pn uid fn = do
  subscribe (pn { channel=(B.concat [(channel pn), "-pnpres"]) }) (Just uid) (fn)

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

leave :: PN -> UUID -> IO ()
leave pn uid = do
  req <- buildRequest pn [ "v2"
                         , "presence"
                         , "sub-key"
                         , (sub_key pn)
                         , "channel"
                         , (channel pn)
                         , "leave"] [("uuid", uid)]
  _ <- withManager $ httpLbs req
  return ()

getUuid :: IO B.ByteString
getUuid =
  B.pack . U.toString <$> nextRandom

unsubscribe :: PN -> IO ()
unsubscribe _ = do
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
