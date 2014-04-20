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

         -- PAM API functions
       , audit
       , auth
       , grant
       ) where

import Network.Pubnub.Types

import Data.Default (def)
import Data.Aeson
import Data.UUID.V4
import Network.HTTP.Conduit
import Network.HTTP.Types
import Control.Monad.Trans
import Control.Concurrent.Async
import Control.Applicative ((<$>))
import Control.Exception.Lifted (try)
import Data.Text.Encoding
import Data.Maybe
import Data.Time.Clock.POSIX

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
  let req = buildRequest defaultPN ["time", "0"] [] Nothing
  res <- withManager $ httpLbs req
  return (decode $ responseBody res :: Maybe Timestamp)

subscribe :: (FromJSON b) => PN -> SubscribeOptions b -> IO (Async ())
subscribe pn subOpts =
    case onPresence subOpts of
      Nothing -> subscribeInternal pn subOpts
      Just onPresenceCallback ->
          case uuid_key pn of
            Nothing -> getUuid >>= \u -> subscribe' pn{uuid_key = Just u}
            _       -> subscribe' pn
          where
            subscribe' pn' =
                do
                  a <- presence pn' (uuid_key pn') onPresenceCallback
                  b <- subscribeInternal pn' subOpts
                  link2 a b
                  return a

subscribeInternal :: (FromJSON b) => PN -> SubscribeOptions b -> IO (Async ())
subscribeInternal pn subOpts =
  async (withManager $ \manager -> lift $ connect pn{time_token = Timestamp 0} manager False)
  where
    connect pn' manager isReconnect = do
      let req = buildSubscribeRequest pn' "0"
      eres <- try $ httpLbs req manager :: IO (Either HttpException (Response L.ByteString))
      case eres of
        Right res ->
          case decode $ responseBody res of
            Just (ConnectResponse ([], t)) -> do
              liftIO (if isReconnect
                      then onReconnect subOpts
                      else onConnect subOpts)
              subscribe' manager (if resumeOnReconnect subOpts && isReconnect
                                  then pn
                                  else pn{time_token=t})
            _ -> do
              liftIO (onDisconnect subOpts)
              subscribe' manager pn
        Left (StatusCodeException (Status code msg) _ _) -> do
          liftIO $ onError subOpts (Just code) (Just msg)
          connect pn' manager isReconnect
        Left _ -> do
          liftIO $ onError subOpts Nothing Nothing
          connect pn' manager isReconnect

    subscribe' manager pn' = do
      let req = buildSubscribeRequest pn' $ head . L.toChunks $ encode (time_token pn')
      eres <- try $ httpLbs req manager :: IO (Either HttpException (Response L.ByteString))
      case eres of
        Right res ->
          case (ctx pn', iv pn') of
            (Just c, Just i) ->
              case decode $ responseBody res of
                Just (EncryptedSubscribeResponse (resp, t)) -> do
                  _ <- liftIO $ mapM (onMsg subOpts . decodeEncrypted c i) resp
                  subscribe' manager (pn' { time_token=t })
                Nothing ->
                  subscribe' manager pn'
            (_, _) ->
              case decode $ responseBody res of
                Just (SubscribeResponse (resp, t)) -> do
                  _ <- liftIO $ mapM (onMsg subOpts) resp
                  subscribe' manager (pn' { time_token=t })
                Nothing ->
                  subscribe' manager pn'
        Left (ResponseTimeout :: HttpException) ->
          subscribe' manager pn'
        Left (StatusCodeException (Status code msg) _ _) -> do
          liftIO $ onError subOpts (Just code) (Just msg)
          reconnect pn' manager
        Left _ -> do
          liftIO $ onError subOpts Nothing Nothing
          reconnect pn' manager

    reconnect pn' manager = connect pn' manager True

    buildSubscribeRequest pn' tt =
      buildRequest pn' [ "subscribe"
                       , encodeUtf8 $ sub_key pn'
                       , encodeUtf8 $ T.intercalate "," (channels pn')
                       , bsFromInteger $ jsonp_callback pn'
                       , tt ]
      (userIdOptions pn')
      (subTimeout subOpts)

    decodeEncrypted c i m = decodeUnencrypted $ decrypt c i m

    decodeUnencrypted :: (FromJSON b) => B.ByteString -> b
    decodeUnencrypted m = fromJust $ decode $ L.fromStrict m

    decrypt c i m = unpadPKCS5 $ decryptCBC c i $ B64.decodeLenient $ decodeJson m

    decodeJson :: T.Text -> B.ByteString
    decodeJson s = case decode (L.fromStrict (encodeUtf8 s)) of
                       Nothing -> encodeUtf8 s
                       Just l -> encodeUtf8 l

publish :: ToJSON a => PN -> T.Text -> a -> IO (Maybe PublishResponse)
publish pn channel msg = do
  let encoded_msg = head . L.toChunks $ encode msg
  let sig = signature (sec_key pn) encoded_msg
  let req = buildRequest pn [ "publish"
                            , encodeUtf8 $ pub_key pn
                            , encodeUtf8 $ sub_key pn
                            , sig
                            , encodeUtf8 channel
                            , bsFromInteger $ jsonp_callback pn
                            , encrypt (ctx pn) (iv pn) encoded_msg]
            (userIdOptions pn)
            Nothing
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)
  where
    signature "0"    _ = "0"
    signature secret m = B.pack $ showDigest $ hmacSha256 (L.fromStrict $ encodeUtf8 secret) (L.fromStrict m)

    encrypt (Just c) (Just i) m = encodeJson $ B64.encode $ encryptCBC c i (padPKCS5 16 m)
    encrypt Nothing  _        m = m
    encrypt _       Nothing   m = m

    encodeJson s = L.toStrict $ encode (decodeUtf8 s)

hereNow :: PN -> T.Text -> IO (Maybe HereNow)
hereNow pn channel = do
  let req = buildRequest pn [ "v2"
                            , "presence"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 channel] [] Nothing
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

presence :: (FromJSON b) => PN -> Maybe UUID -> (b -> IO ()) -> IO (Async ())
presence pn u fn =
  let subOpts = defaultSubscribeOptions{ onMsg = fn }
      pn'     = pn { uuid_key=u, ctx=Nothing, channels=presence_channels }
  in
   subscribeInternal pn' subOpts
  where
    presence_channels = map (prepend "-pnpres") (channels pn)
    prepend           = flip T.append

history :: FromJSON b => PN -> T.Text -> HistoryOptions -> IO (Maybe (History b))
history pn channel options = do
  let req = buildRequest pn [ "v2"
                            , "history"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 channel]
            ((convertHistoryOptions options) ++
             (userIdOptions pn))
            Nothing
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)

leave :: PN -> T.Text -> UUID -> IO ()
leave pn channel u = do
  let req = buildRequest pn [ "v2"
                            , "presence"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 channel
                            , "leave"] [("uuid", encodeUtf8 u)] Nothing
  _ <- withManager $ httpLbs req
  return ()

getUuid :: IO UUID
getUuid =
  T.pack . U.toString <$> nextRandom

unsubscribe :: Async () -> IO ()
unsubscribe = cancel


-- PAM functions

audit :: PN -> Auth -> IO (Maybe Value)
audit = pamDo "audit"

grant :: PN -> Auth -> IO (Maybe Value)
grant = pamDo "grant"

auth :: PN -> T.Text -> PN
auth pn k = pn{auth_key = (Just k)}

pamDo :: B.ByteString -> PN -> Auth -> IO (Maybe Value)
pamDo pamMethod pn authR = do  
  ts <- (bsFromInteger . round) <$> getPOSIXTime      
  let req = buildRequest pn pamURI ((pamQS ts) ++ [("signature", signature ts)]) Nothing  
  res <- withManager $ httpLbs req
  return (decode $ responseBody res)  
  where
    authK = T.intercalate "," $ authKeys authR
    channel = fromJust $ chan authR
    authRead = r authR
    authWrite = w authR
    
    pubKey = encodeUtf8 $ pub_key pn
    subKey = encodeUtf8 $ sub_key pn
    secKey = L.fromStrict . encodeUtf8 $ sec_key pn
        
    signature ts = B64.encode . L.toStrict . bytestringDigest $ hmacSha256 secKey (msg ts)

    msg ts = L.fromStrict $ B.intercalate B.empty [ subKey, "\n"
                                                  , pubKey, "\n"
                                                  , pamMethod, "\n"
                                                  , B.tail $ renderSimpleQuery True $ pamQS ts]

    pamURI = [ "v1"
             , "auth"                            
             , pamMethod
             , "sub-key"
             , subKey]

    pamQS ts = [ ("auth", encodeUtf8 authK)
               , ("channel", encodeUtf8 channel)
               , ("r", if authRead then "1" else "0")
               , ("timestamp", ts)
               , ("w", if authWrite then "1" else "0")]


-- internal functions
userIdOptions :: PN -> [(B.ByteString, B.ByteString)]
userIdOptions pn =
  (maybe [] (\u -> [("uuid", encodeUtf8 u)]) (uuid_key pn)) ++
  (maybe [] (\a -> [("auth", encodeUtf8 a)]) (auth_key pn))

buildRequest :: PN -> [B.ByteString] -> SimpleQuery -> Maybe Int -> Request
buildRequest pn elems qs timeout =
  def { host            = encodeUtf8 $ origin pn
      , path            = B.intercalate "/" elems
      , method          = "GET"
      , port            = if ssl pn then 443 else 80
      , requestHeaders  = [ ("V", "3.1")
                         , ("User-Agent", "Haskell")
                         , ("Accept", "*/*")]
      , queryString     = renderSimpleQuery True qs      
      , secure          = ssl pn
      , responseTimeout = Just (maybe 5000000 (* 1000000) timeout) }

bsFromInteger :: Integer -> B.ByteString
bsFromInteger = B.pack . show
