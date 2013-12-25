{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Pubnub.Types
       (
         -- Record construction
         Timestamp(..)
       , PN(..)
       , defaultPN
       , SubscribeResponse(..)
       , PublishResponse(..)
       , UUID
       , Presence(..)
       , HereNow(..)
       ) where

import GHC.Generics

import Control.Applicative ((<$>), pure, empty)
import Data.Text.Read

import Data.Aeson
import Data.Aeson.TH

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

data PN = PN { origin         :: B.ByteString
             , pub_key        :: B.ByteString
             , sub_key        :: B.ByteString
             , sec_key        :: B.ByteString
             , channel        :: B.ByteString
             , jsonp_callback :: Integer
             , time_token     :: Timestamp }

defaultPN :: PN
defaultPN = PN { origin         = "pubsub.pubnub.com"
               , pub_key        = B.empty
               , sub_key        = B.empty
               , sec_key        = "0"
               , channel        = B.empty
               , jsonp_callback = 0
               , time_token     = Timestamp 0 }

newtype Timestamp = Timestamp Integer
                  deriving (Show)

instance ToJSON Timestamp where
  toJSON (Timestamp t) = (Number . fromIntegral) t

instance FromJSON Timestamp where
  parseJSON (String s) = Timestamp <$> (pure . decimalRight) s
  parseJSON (Array a)  = Timestamp <$> (withNumber "Integral" $ pure . floor) (a V.! 0)
  parseJSON _          = empty

data PublishResponse = PublishResponse Integer String Timestamp
                     deriving (Show, Generic)

instance FromJSON PublishResponse

data SubscribeResponse a = SubscribeResponse (a, Timestamp)
                         deriving (Show, Generic)

instance (FromJSON a) => FromJSON (SubscribeResponse a)

type UUID = B.ByteString
type Occupancy = Integer

data Presence = Presence { action            :: B.ByteString
                         , timestamp         :: Integer
                         , uuid              :: UUID
                         , presenceOccupancy :: Occupancy }
              deriving (Show)

data HereNow = HereNow { uuids            :: [UUID]
                       , herenowOccupancy :: Occupancy }
             deriving (Show)

decimalRight :: T.Text -> Integer
decimalRight x =
  case decimal x of
    Right (i, "") -> i
    _             -> 0


$(deriveJSON defaultOptions{ fieldLabelModifier=(\x -> case x of
                                                    "presenceOccupancy" -> "occupancy"
                                                    _ -> x
                                                    ) } ''Presence)

$(deriveJSON defaultOptions{ fieldLabelModifier=(\x -> case x of
                                                    "herenowOccupancy" -> "occupancy"
                                                    _ -> x
                                                    ) } ''HereNow)
