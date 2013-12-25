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

subscribe :: FromJSON b => PN -> (b -> IO ()) -> IO ()
subscribe pn fn = do
  req <- buildRequest pn ["subscribe", (sub_key pn), (channel pn)
                         , bsFromInteger $ jsonp_callback pn
                         , head . L.toChunks $ encode (time_token pn)]
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
  req <- buildRequest pn ["publish", (pub_key pn), (sub_key pn), (sec_key pn), (channel pn)
                         , bsFromInteger $ jsonp_callback pn
                         , head . L.toChunks $ encode msg]
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

buildRequest :: PN -> [B.ByteString] -> IO Request
buildRequest pn elems = do
  req <- parseUrl "http://"
  return req { host   = (origin pn)
             , path   = B.intercalate "/" elems
             , method = "GET"
             , secure = False
             , port   = 80 }

bsFromInteger :: Integer -> B.ByteString
bsFromInteger = B.pack . show
