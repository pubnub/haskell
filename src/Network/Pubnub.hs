{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Pubnub
       (
         PN(..)
       , defaultPN
       , Timestamp(..)

         -- API function
       , timestamp
       , subscribe
       , publish
       ) where

import Network.Pubnub.Types


import Data.Aeson
import Network.HTTP.Conduit
import Control.Monad.Trans
import Control.Exception.Lifted (try)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

timestamp :: IO (Maybe Timestamp)
timestamp = do
  req <- buildRequest (defaultPN) ["time", "0"]
  res <- withManager $ httpLbs req
  return (decode $ responseBody res :: Maybe Timestamp)

subscribe :: FromJSON b => PN a b -> IO (Maybe (SubscribeResponse b))
subscribe pn = do
  req <- buildRequest pn ["subscribe", (sub_key pn), (channel pn)
                         , bsFromInteger $ jsonp_callback pn
                         , head . L.toChunks $ encode (time_token pn)]
  withManager $ \manager -> do
    eres <- try $ httpLbs req manager
    case eres of
      Right r -> return (decode $ responseBody r)
      Left (ResponseTimeout :: HttpException) -> lift $ subscribe pn
      Left _ -> return Nothing

publish :: PN a b -> a -> IO (Maybe PublishResponse)
publish pn msg = do
  req <- buildRequest pn ["publish", (pub_key pn), (sub_key pn), (sec_key pn), (channel pn)
                         , bsFromInteger $ jsonp_callback pn
                         , head . L.toChunks $ encodeJson pn msg]
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

buildRequest :: PN a b -> [B.ByteString] -> IO Request
buildRequest pn elems = do
  req <- parseUrl "http://"
  return req { host   = (origin pn)
             , path   = B.intercalate "/" elems
             , method = "GET"
             , secure = False
             , port   = 80 }

bsFromInteger :: Integer -> B.ByteString
bsFromInteger = B.pack . show
