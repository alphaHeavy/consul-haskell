{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Consul.Types (
  Check(..),
  Config(..),
  Consistency(..),
  ConsulClient(..),
  Datacenter (..),
  Health(..),
  HealthCheck(..),
  HealthCheckStatus(..),
  Network.Consul.Types.KeyValue(..),
  KeyValuePut(..),
  Member(..),
  Node(..),
  RegisterRequest(..),
  RegisterHealthCheck(..),
  RegisterService(..),
  Self(..),
  Service(..),
  ServiceResult(..),
  Session(..),
  SessionBehavior(..),
  SessionInfo(..),
  SessionRequest(..),
  Sequencer(..)
) where
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Foldable
import Data.Int
import Data.Text(Text)
import qualified Data.Text.Encoding as TE
import Data.Word
import Network.HTTP.Client (Manager)
import Network.Socket

data ConsulClient = ConsulClient{
  ccManager :: Manager,
  ccHostname :: Text,
  ccPort :: PortNumber,
  ccWithTls :: Bool
}

data Datacenter = Datacenter Text deriving (Eq,Show,Ord)

data Consistency = Consistent | Default | Stale deriving (Eq,Show,Enum,Ord)

data HealthCheckStatus = Critical | Passing | Unknown | Warning deriving (Eq,Show,Enum,Ord)

data SessionBehavior = Release | Delete deriving (Eq,Show,Enum,Ord)

data HealthCheck = Script Text Text | Ttl Text | Http Text deriving (Eq,Show,Ord)

data KeyValue = KeyValue {
  kvCreateIndex :: Word64,
  kvLockIndex :: Word64,
  kvModifyIndex :: Word64,
  kvValue :: Maybe ByteString,
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
  seAddress :: Maybe Text,
  sePort :: Maybe Int
} deriving (Show)

data ServiceResult = ServiceResult{
  srrNode :: Text,
  srrAddress :: Text,
  srrServiceId :: Text,
  srrServiceName :: Text,
  srrServiceTags :: [Text],
  srrServiceAddress :: Maybe Text,
  srrServicePort :: Maybe Int
} deriving (Show)

data Check = Check {
  cNode :: Text,
  cCheckId :: Text,
  cName :: Maybe Text,
  cNotes :: Maybe Text,
  cServiceId :: Maybe Text,
  cStatus :: HealthCheckStatus,
  cOutput :: Text,
  cServiceName :: Maybe Text
} deriving (Show)

data Node = Node {
  nNode :: Text,
  nAddress :: Text
} deriving (Show)

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
  rsCheck :: Maybe HealthCheck
}

data Self = Self{
  sMember :: Member
} deriving (Show)

data Config = Config{
  cBootstrap :: Bool,
  cServer :: Bool,
  cDatacenter :: Datacenter,
  cDataDir :: Text,
  cClientAddr :: Text
}

data Member = Member{
  mName :: Text,
  mAddress :: Text,
  mPort :: Int ,
  mTags :: Object,
  mStatus :: Int,
  mProtocolMin :: Int,
  mProtocolMax :: Int,
  mProtocolCur :: Int,
  mDelegateMin :: Int,
  mDelegateMax :: Int,
  mDelegateCur :: Int
} deriving (Show)

{- Health -}
data Health = Health {
  hNode :: Node,
  hService :: Service,
  hChecks :: [Check]
} deriving (Show)


{- JSON Instances -}
instance FromJSON Self where
  parseJSON (Object v) = Self <$> v .: "Member"
  parseJSON _ = mzero

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "Bootstrap" <*> v .: "Server" <*> v .: "Datacenter" <*> v .: "DataDir" <*> v .: "ClientAddr"
  parseJSON _ = mzero

instance FromJSON Member where
  parseJSON (Object v) = Member <$> v .: "Name" <*> v .: "Addr" <*> v .: "Port" <*> v .: "Tags" <*> v .: "Status" <*> v .: "ProtocolMin" <*> v .: "ProtocolMax" <*> v .: "ProtocolCur" <*> v .: "DelegateMin" <*> v .: "DelegateMax" <*> v .: "DelegateCur"
  parseJSON _ = mzero

instance FromJSON HealthCheckStatus where
  parseJSON (String "critical") = pure Critical
  parseJSON (String "passing") = pure Passing
  parseJSON (String "unknown") = pure Unknown
  parseJSON (String "warning") = pure Warning
  parseJSON _ = mzero

instance FromJSON Network.Consul.Types.KeyValue where
  parseJSON (Object v) =
    Network.Consul.Types.KeyValue
      <$> v .: "CreateIndex"
      <*> v .: "LockIndex"
      <*> v .: "ModifyIndex"
      <*> (foo =<< (v .:? "Value"))
      <*> v .: "Flags"
      <*> v .:? "Session"
      <*> v .: "Key"
  parseJSON _ = mzero

instance FromJSON Datacenter where
  parseJSON (String val) = pure $ Datacenter val
  parseJSON _ = mzero

instance FromJSON Check where
  parseJSON (Object x) = Check <$> x .: "Node" <*> x .: "CheckID" <*> x .: "Name" <*> x .:? "Notes" <*> x .:? "ServiceID" <*> x .: "Status" <*> x .: "Output" <*> x .:? "ServiceName"
  parseJSON _ = mzero

instance FromJSON Service where
  parseJSON (Object x) = Service <$> x .: "ID" <*> x .: "Service" <*> x .: "Tags" <*> x .:? "Address" <*> x .:? "Port"
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
  parseJSON _ = mzero

instance FromJSON SessionBehavior where
  parseJSON (String "release") = pure Release
  parseJSON (String "delete") = pure Delete
  parseJSON _ = mzero

instance ToJSON SessionBehavior where
  toJSON Release = String "release"
  toJSON Delete = String "delete"

instance ToJSON RegisterHealthCheck where
  toJSON (RegisterHealthCheck i name notes script interval ttl) = object ["id" .= i, "name" .= name, "notes" .= notes, "script" .= script, "interval" .= interval, "ttl" .= ttl]

instance ToJSON RegisterService where
  toJSON (RegisterService i name tags port check) = object ["ID" .= i, "Name" .= name, "tags" .= tags, "port" .= port, "Check" .= check]

instance ToJSON HealthCheck where
  toJSON (Ttl x) = object ["TTL" .= x]
  toJSON (Http x) = object ["HTTP" .= x]
  toJSON (Script x y) = object ["Script" .= x, "Interval" .= y]

instance ToJSON SessionRequest where
  toJSON (SessionRequest lockDelay name node checks behavior ttl) = object["LockDelay" .= lockDelay, "Name" .= name, "Node" .= (fmap nNode node), "Checks" .= checks, "Behavior" .= behavior, "TTL" .= ttl]

instance ToJSON ServiceResult where
  toJSON (ServiceResult node addr sid sName sTags sAddress sPort) = object["Node" .= node, "Address" .= addr, "ServiceID" .= sid, "ServiceName" .= sName, "ServiceTags" .= sTags, "ServiceAddress" .= sAddress, "ServicePort" .= sPort]

instance FromJSON ServiceResult where
  parseJSON (Object x) = ServiceResult <$> x .: "Node" <*> x .: "Address" <*> x .: "ServiceID" <*> x .: "ServiceName" <*> x .: "ServiceTags" <*> x .:? "ServiceAddress" <*> x .:? "ServicePort"
  parseJSON _ = mzero

foo :: Maybe Value -> Parser (Maybe ByteString)
foo (Just (String x)) =
  case B64.decode $ TE.encodeUtf8 x of
    Left y -> fail y
    Right y -> return $ Just y
foo (Just _) = return Nothing
foo Nothing = return Nothing
