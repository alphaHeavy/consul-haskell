{-# LANGUAGE OverloadedStrings #-}
module Network.Consul.Types (
  Datacenter (..),
  KeyValue(..),
  KeyValuePut(..),
  KeyValueRequest(..),
  Session(..)
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text(Text)
import qualified Data.Text.Encoding as TE
import Data.Word
import Debug.Trace
import Network.Socket

data Datacenter = Datacenter Text deriving (Eq,Show,Ord)

data Consistency = Consistent | Default | Stale deriving (Eq,Show,Enum,Ord)

data HealthCheckStatus = Critical | Passing | Unknown | Warning deriving (Eq,Show,Enum,Ord)

data KeyValue = KeyValue {
  kvCreateIndex :: Word64,
  kvLockIndex :: Word64,
  kvModifyIndex :: Word64,
  kvValue :: ByteString,
  kvFlags :: Word64,
  kvSession :: Maybe Text,
  kvKey :: Text
} deriving (Show)

data KeyValuePut = KeyValuePut {
  kvpKey :: Text,
  kvpValue :: ByteString
}

data KeyValueRequest = KeyValueRequest {
  kvrKey :: Text,
  kvrConsistency :: Maybe Consistency,
  kvrDatacenter :: Maybe Datacenter
}

data Session = Session {
  sCreateIndex :: Word64,
  sId :: Text
}

data RegisterRequest = RegisterRequest {
  rrDatacenter :: Maybe Datacenter,
  rrNode :: Text,
  rrAddress :: Text,
  rrService :: Maybe Service,
  rrCheck :: Maybe Check
}

data Service = Service {
  seId :: Text,
  seService :: Text,
  seTags :: [Text],
  sePort :: Maybe Int
}

data Check = Check {
  cNode :: Text,
  cCheckId :: Text,
  cName :: Maybe Text,
  cNotes :: Maybe Text,
  cServiceId :: Maybe Text,
  cStatus :: HealthCheckStatus
}

{- JSON Instances -}
instance FromJSON KeyValue where
  parseJSON (Object v) = KeyValue <$> v .: "CreateIndex" <*> v .: "LockIndex" <*> v .: "ModifyIndex" <*> (foo =<< B64.decode . TE.encodeUtf8 <$> v .: "Value") <*> v .: "Flags" <*> v .:? "Session" <*> v .: "Key"
  parseJSON _ = mzero

instance FromJSON Datacenter where
  parseJSON (String val) = pure $ Datacenter val
  parseJSON _ = mzero

foo :: Monad m => Either String a -> m a
foo (Left x) = trace "failing" $ fail x
foo (Right x) = return x
