{-# LANGUAGE OverloadedStrings #-}
{-# Language DeriveDataTypeable #-}
module Network.Consul.Types (
  Check(..),
  ConsulClient(..),
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
import Data.Data
import Data.Text(Text)
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Data.Word
import Debug.Trace
import Network.HTTP.Client (Manager)
import Network.Socket

data ConsulClient = ConsulClient{
  ccManager :: Manager,
  ccHostname :: Text,
  ccPort :: PortNumber
}

data Datacenter = Datacenter Text deriving (Eq,Show,Ord)

data Consistency = Consistent | Default | Stale deriving (Eq,Show,Enum,Ord)

data HealthCheckStatus = Critical | Passing | Unknown | Warning deriving (Data,Eq,Show,Enum,Ord,Typeable)

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
  cStatus :: HealthCheckStatus,
  cOutput :: Text,
  cServiceName :: Text
}

data Node = Node {
  nNode :: Text,
  nNodeAddress :: SockAddr
}

{- Health -}


{- JSON Instances -}
instance FromJSON HealthCheckStatus where
  parseJSON (String "Critical") = pure Critical
  parseJSON (String "Passing") = pure Passing
  parseJSON (String "Unknown") = pure Unknown
  parseJSON (String "Warning") = pure Warning
  parseJSON _ = mzero

instance FromJSON KeyValue where
  parseJSON (Object v) = KeyValue <$> v .: "CreateIndex" <*> v .: "LockIndex" <*> v .: "ModifyIndex" <*> (foo =<< B64.decode . TE.encodeUtf8 <$> v .: "Value") <*> v .: "Flags" <*> v .:? "Session" <*> v .: "Key"
  parseJSON _ = mzero

instance FromJSON Datacenter where
  parseJSON (String val) = pure $ Datacenter val
  parseJSON _ = mzero

instance FromJSON Check where
  parseJSON (Object x) = Check <$> x .: "Node" <*> x .: "CheckId" <*> x .: "Name" <*> x .: "Notes" <*> x .: "ServiceId" <*> x .: "Status" <*> x .: "Output" <*> x .: "ServiceName"
  parseJSON _ = mzero

foo :: Monad m => Either String a -> m a
foo (Left x) = trace "failing" $ fail x
foo (Right x) = return x
