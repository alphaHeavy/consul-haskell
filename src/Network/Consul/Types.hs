{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Consul.Types (
  Check(..),
  Consistency(..),
  ConsulClient(..),
  Datacenter (..),
  Health(..),
  KeyValue(..),
  KeyValuePut(..),
  RegisterRequest(..),
  RegisterHealthCheck(..),
  RegisterService(..),
  Session(..),
  SessionBehavior(..),
  SessionInfo(..),
  SessionRequest(..),
  Sequencer(..)
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Text(Text)
import qualified Data.Text.Encoding as TE
import Data.Traversable
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

data HealthCheckStatus = Critical | Passing | Unknown | Warning deriving (Eq,Show,Enum,Ord)

data SessionBehavior = Release | Delete deriving (Eq,Show,Enum,Ord)

data KeyValue = KeyValue {
  kvCreateIndex :: Word64,
  kvLockIndex :: Word64,
  kvModifyIndex :: Word64,
  kvValue :: ByteString,
  kvFlags :: Word64,
  kvSession :: Maybe Text,
  kvKey :: Text
} deriving (Show,Eq)

data KeyValuePut = KeyValuePut {
  kvpKey :: Text,
  kvpValue :: ByteString,
  kvpCasIndex :: Maybe Word64,
  kvpFlags :: Maybe Word64
}

data Session = Session {
  sId :: Text,
  sCreateIndex :: Maybe Word64
} deriving (Show)

data SessionInfo = SessionInfo {
  siLockDelay :: Maybe Word64,
  siChecks :: [Text],
  siNode :: Text,
  siId :: Text,
  siBehavior :: Maybe SessionBehavior,
  siCreateIndex :: Word64,
  siName :: Maybe Text,
  siTtl :: Maybe Text
} deriving (Eq,Show)

newtype SessionInfoList = SessionInfoList [SessionInfo]

data SessionRequest = SessionRequest {
  srLockDelay :: Maybe Text,
  srName :: Maybe Text,
  srNode :: Maybe Node,
  srChecks :: [Text],
  srBehavor :: Maybe SessionBehavior,
  srTtl :: Maybe Text
}

data Sequencer = Sequencer{
  sKey :: Text,
  sLockIndex :: Word64,
  sSession :: Session
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
  nAddress :: Text
}

{- Agent -}
data RegisterHealthCheck = RegisterHealthCheck {
  rhcId :: Text,
  rhcName :: Text,
  rhcNotes :: Text,
  rhcScript :: Maybe Text,
  rhcInterval :: Maybe Text,
  rhcTtl :: Maybe Text
}

data RegisterService = RegisterService {
  rsId :: Maybe Text,
  rsName :: Text,
  rsTags :: [Text],
  rsPort :: Maybe Int16,
  rsCheck :: Either (Text,Text) Text -- (script,interval) ttl
}

{- Health -}
data Health = Health {
  hNode :: Node,
  hService :: Service,
  hChecks :: [Check]
}


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

instance FromJSON Service where
  parseJSON (Object x) = Service <$> x .: "Id" <*> x .: "Service" <*> x .: "Tags" <*> x .: "Port"
  parseJSON _ = mzero

instance FromJSON Node where
  parseJSON (Object x) = Node <$> x .: "Node" <*> x .: "Address"
  parseJSON _ = mzero

instance FromJSON Health where
  parseJSON (Object x) = Health <$> x.: "Node" <*> x .: "Service" <*> x .: "Checks"
  parseJSON _ = mzero

instance FromJSON Session where
  parseJSON (Object x) = Session <$> x .: "ID" <*> pure Nothing
  parseJSON _ = mzero

instance FromJSON SessionInfoList where
  parseJSON (Array x) = SessionInfoList <$> traverse parseJSON (toList x)
  parseJSON _ = mzero

instance FromJSON SessionInfo where
  parseJSON (Object x) = SessionInfo <$> x .:? "LockDelay" <*> x .: "Checks" <*> x .: "Node" <*> x .: "ID" <*> x .:? "Behavior" <*> x .: "CreateIndex" <*> x .:? "Name" <*> x .:? "TTL"
  --parseJSON (Object x) = SessionInfo <$> x .: "CreateIndex"
  parseJSON _ = mzero

instance FromJSON SessionBehavior where
  parseJSON (String "release") = pure Release
  parseJSON (String "delete") = pure Delete

instance ToJSON SessionBehavior where
  toJSON Release = String "release"
  toJSON Delete = String "delete"

instance ToJSON RegisterHealthCheck where
  toJSON (RegisterHealthCheck id name notes script interval ttl) = object ["id" .= id, "name" .= name, "notes" .= notes, "script" .= script, "interval" .= interval, "ttl" .= ttl]

instance ToJSON RegisterService where
  toJSON (RegisterService id name tags port check) = object ["ID" .= id, "Name" .= name, "Tags" .= tags, "Port" .= port, "Check" .= check]

instance ToJSON SessionRequest where
  toJSON (SessionRequest lockDelay name node checks behavior ttl) = object["LockDelay" .= lockDelay, "Name" .= name, "Node" .= (fmap nNode node), "Checks" .= checks, "Behavior" .= behavior, "TTL" .= ttl]

instance ToJSON (Either (Text,Text) Text) where
  toJSON (Left (script,interval)) = object ["Script" .= script, "Interval" .= interval]
  toJSON (Right ttl) = object ["TTL" .= ttl]

foo :: Monad m => Either String a -> m a
foo (Left x) = trace "failing" $ fail x
foo (Right x) = return x
