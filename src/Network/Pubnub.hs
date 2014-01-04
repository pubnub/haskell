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

import Data.Default (def)
import Data.Aeson
import Data.UUID.V4
import Network.HTTP.Conduit
import Control.Monad.Trans
import Network.HTTP.Types.URI
import Control.Concurrent.Async
import Control.Applicative ((<$>))
import Control.Exception.Lifted (try)
import Data.Text.Encoding
import Data.Maybe

import Data.Digest.Pure.SHA
import Crypto.Cipher.AES
import Crypto.Padding

import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as B64

time :: IO (Maybe Timestamp)
time = do
  let req = buildRequest defaultPN ["time", "0"] []
  res <- withManager $ httpLbs req
  return (decode $ responseBody res :: Maybe Timestamp)

subscribe :: (FromJSON b) => PN -> SubscribeOptions b -> Maybe UUID -> IO (Async ())
subscribe pn subOpts uid = do
  async (withManager $ \manager -> connect manager >>= subscribe' manager)
  where
    connect manager = do
      let req = buildRequest pn [ "subscribe"
                                 , encodeUtf8 $ sub_key pn
                                 , encodeUtf8 $ T.intercalate "," (channels pn)
                                 , bsFromInteger $ jsonp_callback pn
                                 , "0"] (case uid of
                                            Just u -> [("uuid", encodeUtf8 u)]
                                            Nothing -> [])

      res <- httpLbs req manager
      case decode $ responseBody res of
        Just (ConnectResponse ([], t)) -> do
          liftIO (onConnect subOpts)
          return pn{time_token=t}
        _ ->
          return pn

    subscribe' manager pn' = do
      let req = buildRequest pn' [ "subscribe"
                                 , encodeUtf8 $ sub_key pn'
                                 , encodeUtf8 $ T.intercalate "," (channels pn')
                                 , bsFromInteger $ jsonp_callback pn'
                                 , head . L.toChunks $ encode (time_token pn')] (case uid of
                                                                                    Just u -> [("uuid", encodeUtf8 u)]
                                                                                    Nothing -> [])

      eres <- try $ httpLbs req manager
      case eres of
        Right r ->
          case (ctx pn', iv pn') of
            (Just c, Just i) ->
              case decode $ responseBody r of
                Just (EncryptedSubscribeResponse (resp, t)) -> do
                  _ <- lift $ mapM ((onMsg subOpts) . decodeEncrypted c i) resp
                  subscribe' manager (pn' { time_token=t })
                Nothing ->
                  subscribe' manager pn'
            (_, _) ->
              case decode $ responseBody r of
                Just (SubscribeResponse (resp, t)) -> do
                  _ <- lift $ mapM (onMsg subOpts) resp
                  subscribe' manager (pn' { time_token=t })
                Nothing ->
                  subscribe' manager pn'
        Left (ResponseTimeout :: HttpException) ->
          subscribe' manager pn'
        Left _ ->
          return ()

    decodeEncrypted c i m = decodeUnencrypted $ decrypt c i m

    decodeUnencrypted :: (FromJSON b) => B.ByteString -> b
    decodeUnencrypted m = fromJust $ decode $ L.fromStrict m

    decrypt c i m = unpadPKCS5 $ decryptCBC c i $ B64.decodeLenient $ decodeJson m

    decodeJson :: B.ByteString -> B.ByteString
    decodeJson s = case decode (L.fromStrict s) of
                       Nothing -> s
                       Just l -> L.toStrict l

publish :: ToJSON a => PN -> T.Text -> a -> IO (Maybe PublishResponse)
publish pn channel msg = do
  let encoded_msg = head . L.toChunks $ encode msg
  let sig = signature (sec_key pn) encoded_msg
  let req = buildRequest pn [ "publish"
                            , encodeUtf8 $ pub_key pn
                            , encodeUtf8 $ sub_key pn
                            , sig
                            , encodeUtf8 $ channel
                            , bsFromInteger $ jsonp_callback pn
                            , encrypt (ctx pn) (iv pn) encoded_msg] []
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)
  where
    signature "0"    _ = "0"
    signature secret m = B.pack $ showDigest $ hmacSha256 (L.fromStrict $ encodeUtf8 secret) (L.fromStrict m)

    encrypt (Just c) (Just i) m = encodeJson $ B64.encode $ encryptCBC c i (padPKCS5 16 m)
    encrypt Nothing     _     m = m
    encrypt   _       Nothing m = m

    encodeJson s = L.toStrict $ encode s

hereNow :: PN -> T.Text -> IO (Maybe HereNow)
hereNow pn channel = do
  let req = buildRequest pn [ "v2"
                            , "presence"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 $ channel] []
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

presence :: (FromJSON b) => PN -> UUID -> (b -> IO ()) -> IO (Async ())
presence pn uid fn =
  subscribe (pn { ctx=Nothing, channels=presence_channels }) defaultSubscribeOptions{onMsg = fn} (Just uid)
  where
    presence_channels = map (prepend "-pnpres") (channels pn)
    prepend = flip T.append

history :: FromJSON b => PN -> T.Text -> HistoryOptions -> IO (Maybe (History b))
history pn channel options = do
  let req = buildRequest pn [ "v2"
                            , "history"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 $ channel] (convertHistoryOptions options)
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

leave :: PN -> T.Text -> UUID -> IO ()
leave pn channel uid = do
  let req = buildRequest pn [ "v2"
                            , "presence"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 channel
                            , "leave"] [("uuid", encodeUtf8 uid)]
  _ <- withManager $ httpLbs req
  return ()

getUuid :: IO UUID
getUuid =
  T.pack . U.toString <$> nextRandom

unsubscribe :: Async () -> IO ()
unsubscribe = cancel

buildRequest :: PN -> [B.ByteString] -> SimpleQuery -> Request
buildRequest pn elems qs =
  def { host           = encodeUtf8 $ origin pn
      , path           = B.intercalate "/" elems
      , method         = "GET"
      , port           = if (ssl pn) then 443 else 80
      , requestHeaders = [ ("V", "3.1")
                         , ("User-Agent", "Haskell")
                         , ("Accept", "*/*")]
      , queryString    = renderSimpleQuery True qs
      , secure         = ssl pn }

bsFromInteger :: Integer -> B.ByteString
bsFromInteger = B.pack . show
