{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Pubnub.Types
       (
         convertHistoryOptions

         -- Record construction
       , Timestamp(..)
       , PN(..)
       , defaultPN
       , SubscribeResponse(..)
       , EncryptedSubscribeResponse(..)
       , PublishResponse(..)
       , UUID
       , Presence(..)
       , Action(..)
       , HereNow(..)
       , History(..)
       , HistoryOption(..)
       , HistoryOptions
       , setEncryptionKey
       ) where

import GHC.Generics

import Control.Applicative ((<$>), pure, empty)
import Data.Text.Read
import Data.Aeson
import Data.Aeson.TH

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Data.Digest.Pure.SHA

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

data PN = PN { origin         :: T.Text
             , pub_key        :: T.Text
             , sub_key        :: T.Text
             , sec_key        :: T.Text
             , channels       :: [T.Text]
             , jsonp_callback :: Integer
             , time_token     :: Timestamp
             , cipher_key     :: B.ByteString
             , ctx            :: Maybe AES
             , iv             :: Maybe (IV AES)
             , ssl            :: Bool}

defaultPN :: PN
defaultPN = PN { origin         = "haskell.pubnub.com"
               , pub_key        = T.empty
               , sub_key        = T.empty
               , sec_key        = "0"
               , channels       = []
               , jsonp_callback = 0
               , time_token     = Timestamp 0
               , cipher_key     = B.empty
               , ctx            = Nothing
               , iv             = makeIV (B.pack "0123456789012345")
               , ssl            = False }

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
    Timestamp <$> (withNumber "Integral" $ pure . floor) (V.head a)
  parseJSON _          = empty

data PublishResponse = PublishResponse Integer String Timestamp
                     deriving (Show, Generic)

instance FromJSON PublishResponse

data SubscribeResponse a = SubscribeResponse (a, Timestamp)
                         deriving (Show, Generic)

instance (FromJSON a) => FromJSON (SubscribeResponse a)

data EncryptedSubscribeResponse = EncryptedSubscribeResponse ([B.ByteString], Timestamp)
                         deriving (Show, Generic)

instance FromJSON EncryptedSubscribeResponse

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

data History a = History [a] Integer Integer
               deriving (Show, Generic)

instance (FromJSON a) => FromJSON (History a)

data HistoryOption = Start Integer
                    | End Integer
                    | Reverse Bool
                    | Count Integer

type HistoryOptions = [HistoryOption]

convertHistoryOptions :: HistoryOptions -> [(B.ByteString, B.ByteString)]
convertHistoryOptions =
  map convertHistoryOption

convertHistoryOption :: HistoryOption -> (B.ByteString, B.ByteString)
convertHistoryOption (Start i)       = ("start", B.pack $ show i)
convertHistoryOption (End i)         = ("end", B.pack $ show i)
convertHistoryOption (Reverse True)  = ("reverse", "true")
convertHistoryOption (Reverse False) = ("reverse", "false")
convertHistoryOption (Count i)       = ("count", B.pack $ show i)

decimalRight :: T.Text -> Integer
decimalRight = either (const 0) fst . decimal

$(deriveJSON defaultOptions{ fieldLabelModifier= \ x -> case x of
                                                    "presenceOccupancy" -> "occupancy"
                                                    _ -> x } ''Presence)

$(deriveJSON defaultOptions{ fieldLabelModifier= \ x -> case x of
                                                    "herenowOccupancy" -> "occupancy"
                                                    _ -> x } ''HereNow)
