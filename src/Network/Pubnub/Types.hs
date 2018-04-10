{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Pubnub.Types
       (
         convertHistoryOptions

         -- Record construction
       , Timestamp(..)
       , PN(..)
       , defaultPN
       , SubscribeOptions(..)
       , defaultSubscribeOptions
       , Auth(..)
       , defaultAuth
       , ConnectResponse(..)
       , SubscribeResponse(..)
       , EncryptedSubscribeResponse(..)
       , PublishResponse(..)
       , ChannelGroupResponse(..)
       , UUID
       , Presence(..)
       , Action(..)
       , HereNow(..)
       , ChannelGroup(..)
       , History(..)
       , HistoryOption(..)
       , HistoryOptions
       , setEncryptionKey
       ) where

import           GHC.Generics

import           Control.Applicative   (empty)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text.Read

import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Data.Digest.Pure.SHA

import           Network.HTTP.Client   (Manager, defaultManagerSettings,
                                        newManager)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as T
import qualified Data.Vector           as V

data PN = PN { origin         :: T.Text
             , pub_key        :: T.Text
             , sub_key        :: T.Text
             , sec_key        :: T.Text
             , uuid_key       :: Maybe UUID
             , auth_key       :: Maybe T.Text
             , channels       :: [T.Text]
             , jsonp_callback :: Integer
             , time_token     :: Timestamp
             , cipher_key     :: B.ByteString
             , ctx            :: Maybe AES
             , iv             :: Maybe (IV AES)
             , ssl            :: Bool
             , pnManager      :: Manager}

defaultPN :: IO PN
defaultPN = do
    man <- newManager defaultManagerSettings
    return PN { origin         = "haskell.pubnub.com"
               , pub_key        = T.empty
               , sub_key        = T.empty
               , sec_key        = "0"
               , uuid_key       = Nothing
               , auth_key       = Nothing
               , channels       = []
               , jsonp_callback = 0
               , time_token     = Timestamp 0
               , cipher_key     = B.empty
               , ctx            = Nothing
               , iv             = makeIV (B.pack "0123456789012345")
               , ssl            = False
               , pnManager      = man }

data SubscribeOptions a = SubscribeOptions { onMsg             :: a -> IO ()
                                           , onConnect         :: IO ()
                                           , onDisconnect      :: IO ()
                                           , onError           :: Maybe Int -> Maybe B.ByteString -> IO ()
                                           , onPresence        :: Maybe (Presence -> IO ())
                                           , onReconnect       :: IO ()

                                           , subTimeout        :: Maybe Int
                                           , resumeOnReconnect :: Bool
                                           , windowing         :: Maybe Integer }

defaultSubscribeOptions :: SubscribeOptions a
defaultSubscribeOptions = SubscribeOptions { onMsg             = \_ -> return ()
                                           , onConnect         = return ()
                                           , onDisconnect      = return ()
                                           , onError           = \_ _ -> return ()
                                           , onPresence        = Nothing
                                           , onReconnect       = return ()

                                           , subTimeout        = Just 310
                                           , resumeOnReconnect = True
                                           , windowing         = Nothing }

data Auth = Auth { chan     :: Maybe T.Text
                 , authKeys :: [T.Text]
                 , r        :: Bool
                 , w        :: Bool
                 , ttl      :: Int }
             deriving (Show)

defaultAuth :: Auth
defaultAuth = Auth { chan     = Nothing
                   , authKeys = []
                   , r        = False
                   , w        = False
                   , ttl      = 0 }

setEncryptionKey :: PN -> B.ByteString -> Either KeyError PN
setEncryptionKey pn key =
  either Left (\a -> Right pn{ctx = Just (initAES256 a)}) (convertKey key)
  where
    initAES256 :: Key AES -> AES
    initAES256 = cipherInit

    convertKey k = makeKey (B.pack . take 32 . showDigest . sha256 $ L.fromStrict k)

newtype Timestamp = Timestamp Integer
                  deriving (Show)

instance ToJSON Timestamp where
  toJSON (Timestamp t) = (Number . fromIntegral) t

instance FromJSON Timestamp where
  parseJSON (String s) = Timestamp <$> (pure . decimalRight) s
  parseJSON (Array a)  =
    Timestamp <$> (withScientific "Integral" $ pure . floor) (V.head a)
  parseJSON _          = empty

newtype ConnectResponse = ConnectResponse ([Value], Timestamp)
                         deriving (Show, Generic)

instance FromJSON ConnectResponse

data PublishResponse = PublishResponse Integer String Timestamp
                     deriving (Show, Generic)

instance FromJSON PublishResponse

newtype SubscribeResponse a = SubscribeResponse ([a], Timestamp)
                         deriving (Show, Generic)

instance (FromJSON a) => FromJSON (SubscribeResponse a)

newtype EncryptedSubscribeResponse = EncryptedSubscribeResponse ([T.Text], Timestamp)
                         deriving (Show, Generic)

instance FromJSON EncryptedSubscribeResponse

data ChannelGroupResponse = ChannelGroupResponse String String Bool String
                     deriving (Show, Generic)

instance FromJSON ChannelGroupResponse

type UUID = T.Text
type Occupancy = Integer

data Action = Join | Leave | Timeout
            deriving (Show)

instance FromJSON Action where
  parseJSON (String "join")    = pure Join
  parseJSON (String "leave")   = pure Leave
  parseJSON (String "timeout") = pure Timeout
  parseJSON _                  = empty

instance ToJSON Action where
  toJSON Join    = String "join"
  toJSON Leave   = String "leave"
  toJSON Timeout = String "timeout"

data Presence = Presence { action            :: Action
                         , timestamp         :: Integer
                         , uuid              :: UUID
                         , presenceOccupancy :: Occupancy }
              deriving (Show)

data HereNow = HereNow { uuids            :: [UUID]
                       , herenowOccupancy :: Occupancy }
             deriving (Show)

data ChannelGroup a = ChannelGroup [a] Integer Integer
               deriving (Show, Generic)

instance (FromJSON a) => FromJSON (ChannelGroup a)

data History a = History [a] Integer Integer
               deriving (Show, Generic)

instance (FromJSON a) => FromJSON (History a)

data HistoryOption = Start Integer
                    | End Integer
                    | Reverse Bool
                    | Count Integer

type HistoryOptions = [HistoryOption]

convertHistoryOptions :: HistoryOptions -> [(B.ByteString, Maybe B.ByteString)]
convertHistoryOptions =
  map convertHistoryOption

convertHistoryOption :: HistoryOption -> (B.ByteString, Maybe B.ByteString)
convertHistoryOption (Start i)       = ("start"  , Just $ B.pack $ show i)
convertHistoryOption (End i)         = ("end"    , Just $ B.pack $ show i)
convertHistoryOption (Reverse True)  = ("reverse", Just   "true")
convertHistoryOption (Reverse False) = ("reverse", Just   "false")
convertHistoryOption (Count i)       = ("count"  , Just $ B.pack $ show i)

decimalRight :: T.Text -> Integer
decimalRight = either (const 0) fst . decimal

$(deriveJSON defaultOptions{ fieldLabelModifier =
                                \ x -> case x of
                                  "presenceOccupancy" -> "occupancy"
                                  _                   -> x } ''Presence)

$(deriveJSON defaultOptions{ fieldLabelModifier = \ x ->
                              case x of
                                "herenowOccupancy" -> "occupancy"
                                _                  -> x } ''HereNow)
