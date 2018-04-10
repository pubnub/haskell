{-# LANGUAGE OverloadedStrings   #-}
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
       , channelGroupList
       , channelGroupAddChannel
       , channelGroupRemoveChannel
       , channelGroupDeleteGroup

         -- PAM API functions
       , audit
       , auth
       , grant
       ) where
import           Control.Monad.Trans.Resource
import           Network.Pubnub.Types

import           Data.Aeson
import           Data.Conduit                 hiding (connect)
import           Data.UUID.V4
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Control.Applicative          ((<$>))
import           Control.Concurrent.Async
import           Control.Exception.Lifted     (try)
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Text.Encoding
import           Data.Time.Clock.POSIX

import           Crypto.Cipher.AES
import           Crypto.Padding
import           Data.Digest.Pure.SHA

import qualified Data.ByteString.Base64       as B64
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as L
import qualified Data.Text                    as T
import qualified Data.UUID                    as U

time :: IO (Maybe Timestamp)
time = do
  pn <- defaultPN
  let req = buildGetRequest pn ["time", "0"] [] Nothing
  res <- httpLbs req (pnManager pn)
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
  async (runResourceT $ connect pn{time_token = Timestamp 0} (pnManager pn) False)
  where
    connect pn' manager isReconnect = do
      let req = buildSubscribeRequest pn' "0"
      eres <- try $ httpLbs req manager
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
        Left (HttpExceptionRequest _ (StatusCodeException res _)) -> do
          let status = responseStatus res
          let code = statusCode status
          let msg  = statusMessage status
          liftIO $ onError subOpts (Just code) (Just msg)
          connect pn' manager isReconnect
        Left _ -> do
          liftIO $ onError subOpts Nothing Nothing
          reconnect pn' manager

    subscribe' manager pn' = do
      let req = buildSubscribeRequest pn' $ L.toStrict $ encode (time_token pn')
      eres <- try $ http req manager
      case eres of
        Right res -> do
          case (ctx pn', iv pn') of
            (Just c, Just i) ->
              responseBody res $$+- encryptedSubscribeSink c i
            (_, _) ->
              responseBody res $$+- subscribeSink
          reconnect pn' manager
        Left (HttpExceptionRequest _ ResponseTimeout) ->
          subscribe' manager pn'
        Left (HttpExceptionRequest _ (StatusCodeException res _)) -> do
          let status = responseStatus res
          let code = statusCode status
          let msg  = statusMessage status
          liftIO $ onError subOpts (Just code) (Just msg)
          reconnect pn' manager
        Left _ -> do
          liftIO $ onError subOpts Nothing Nothing
          reconnect pn' manager

    encryptedSubscribeSink c i =
      awaitForever (\x ->
                       case decode (L.fromStrict x) of
                         Just (EncryptedSubscribeResponse (resp, _)) -> do
                           _ <- liftIO $ mapM (onMsg subOpts . decodeEncrypted c i) resp
                           return ()
                         Nothing ->
                           return ())

    subscribeSink =
      awaitForever (\x ->
                       case decode (L.fromStrict x) of
                         Just (SubscribeResponse (resp, _)) -> do
                           _ <- liftIO $ mapM (onMsg subOpts) resp
                           return ()
                         Nothing ->
                           return ())

    reconnect pn' manager = connect pn' manager True

    buildSubscribeRequest pn' tt =
      buildGetRequest pn' [ "stream"
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
                       Just l  -> encodeUtf8 l

publish :: ToJSON a => PN -> T.Text -> a -> IO (Maybe PublishResponse)
publish pn channel msg = do
  let encoded_msg = L.toStrict $ encode msg
  let sig = signature (sec_key pn) encoded_msg
  let req = buildPostRequest pn [ "publish"
                            , encodeUtf8 $ pub_key pn
                            , encodeUtf8 $ sub_key pn
                            , sig
                            , encodeUtf8 channel
                            , bsFromInteger $ jsonp_callback pn
                            ]
            (encrypt (ctx pn) (iv pn) encoded_msg)
            (userIdOptions pn)
            Nothing

  res <- httpLbs req (pnManager pn)
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
  let req = buildGetRequest pn [ "v2"
                            , "presence"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 channel] [] Nothing
  res <- httpLbs req (pnManager pn)
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
  let req = buildGetRequest pn [ "v2"
                            , "history"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 channel]
            (convertHistoryOptions options ++ userIdOptions pn)
            Nothing
  res <- httpLbs req (pnManager pn)
  return (decode $ responseBody res)

channelGroupList :: FromJSON b => PN -> T.Text -> IO (Maybe (ChannelGroup b))
channelGroupList pn group = do
  let req = buildGetRequest pn [ "v1"
                               , "channel-registration"
                               , "sub-key"
                               , encodeUtf8 $ sub_key pn
                               , "channel-group"
                               , encodeUtf8 group
                               ]
            (userIdOptions pn)
            Nothing
  res <- httpLbs req (pnManager pn)
  return (decode $ responseBody res)

channelGroupAddChannel :: PN -> T.Text -> [T.Text] -> IO (Maybe ChannelGroupResponse)
channelGroupAddChannel = channelGroupDo "add"

channelGroupRemoveChannel :: PN -> T.Text -> [T.Text] -> IO (Maybe ChannelGroupResponse)
channelGroupRemoveChannel = channelGroupDo "remove"

channelGroupDeleteGroup :: PN -> T.Text -> IO (Maybe ChannelGroupResponse)
channelGroupDeleteGroup pn group = channelGroupDo "remove" pn group []

channelGroupDo :: T.Text -> PN -> T.Text -> [T.Text] -> IO (Maybe ChannelGroupResponse)
channelGroupDo action' pn group chs  = do
  let req = buildGetRequest pn [ "v1"
                               , "channel-registration"
                               , "sub-key"
                               , encodeUtf8 $ sub_key pn
                               , "channel-group"
                               , encodeUtf8 group
                               ]
            (channelOptions ++ userIdOptions pn)
            Nothing
  res <- httpLbs req (pnManager pn)
  return (decode $ responseBody res)
  where
    channelOptions ::[(B.ByteString, Maybe B.ByteString)]
    channelOptions
      -- delete groupChannel
      | null chs && action' == "remove" = [("remove", Nothing)]
      -- add/remove channels from channel group
      | otherwise = map (\g -> (encodeUtf8 action', Just $ encodeUtf8 g)) chs

leave :: PN -> T.Text -> UUID -> IO ()
leave pn channel u = do
  let req = buildGetRequest pn [ "v2"
                            , "presence"
                            , "sub-key"
                            , encodeUtf8 $ sub_key pn
                            , "channel"
                            , encodeUtf8 channel
                            , "leave"] [("uuid", Just $ encodeUtf8 u)] Nothing
  _ <- httpLbs req (pnManager pn)
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
auth pn k = pn{auth_key = Just k}

pamDo :: B.ByteString -> PN -> Auth -> IO (Maybe Value)
pamDo pamMethod pn authR = do
  ts <- (bsFromInteger . round) <$> getPOSIXTime
  let req = buildGetRequest pn pamURI (pamQS ts ++ [("signature", Just $ signature ts)]) Nothing
  res <- httpLbs req (pnManager pn)
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
                                                  , B.tail $ renderQuery True $ pamQS ts]

    pamURI = [ "v1"
             , "auth"
             , pamMethod
             , "sub-key"
             , subKey]

    pamQS ts = [ ("auth"     , Just $ encodeUtf8 authK)
               , ("channel"  , Just $ encodeUtf8 channel)
               , ("r"        , Just $ if authRead then "1" else "0")
               , ("timestamp", Just   ts)
               , ("w"        , Just $ if authWrite then "1" else "0")]


-- internal functions
userIdOptions :: PN -> [(B.ByteString, Maybe B.ByteString)]
userIdOptions pn =
  maybe [] (\u -> [("uuid", Just $ encodeUtf8 u)]) (uuid_key pn) ++
  maybe [] (\a -> [("auth", Just $ encodeUtf8 a)]) (auth_key pn)

buildGetRequest :: PN -> [B.ByteString] -> Query -> Maybe Int -> Request
buildGetRequest pn elems qs timeout =
  defaultRequest
      { host            = encodeUtf8 $ origin pn
      , path            = B.intercalate "/" elems
      , method          = "GET"
      , port            = if ssl pn then 443 else 80
      , requestHeaders  = [ ("V", "3.1")
                         , ("User-Agent", "Haskell")
                         , ("Accept", "*/*")]
      , queryString     = renderQuery True qs
      , secure          = ssl pn
      , responseTimeout = responseTimeoutMicro (maybe 5000000 (* 1000000) timeout) }

buildPostRequest :: PN -> [B.ByteString] -> B.ByteString -> Query -> Maybe Int -> Request
buildPostRequest pn elems body qs timeout =
  defaultRequest
      { host            = encodeUtf8 $ origin pn
      , path            = B.intercalate "/" elems
      , method          = "POST"
      , port            = if ssl pn then 443 else 80
      , requestHeaders  = [ ("V", "3.1")
                         , ("User-Agent", "Haskell")
                         , ("Accept", "*/*")]
      , queryString     = renderQuery True qs
      , secure          = ssl pn
      , responseTimeout = responseTimeoutMicro (maybe 5000000 (* 1000000) timeout)
      , requestBody = RequestBodyBS body
      }


bsFromInteger :: Integer -> B.ByteString
bsFromInteger = B.pack . show
