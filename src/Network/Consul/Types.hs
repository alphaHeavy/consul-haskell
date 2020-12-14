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
  Sequencer(..),
  ConsulHost,
  ApiEndpoint,
  ConsulQuery,
  ConsulRequestBody,
  WaitFlag,
  noQuery,
  noRequestBody,
  waitTrue,
  waitFalse
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

data ConsulClient = ConsulClient
  { ccManager :: Manager
  , ccHostname :: Text
  , ccPort :: PortNumber
  , ccWithTls :: Bool
  , ccDatacenter :: Maybe Datacenter
  }

data Datacenter = Datacenter Text deriving (Eq, Ord, Show)

data Consistency = Consistent | Default | Stale deriving (Bounded, Enum, Eq, Ord, Show)

data HealthCheckStatus = Critical | Passing | Unknown | Warning deriving (Bounded, Enum, Eq, Ord, Show)

data SessionBehavior = Release | Delete deriving (Bounded, Enum, Eq, Ord, Show)

data HealthCheck = Script Text Text | Ttl Text | Http Text deriving (Eq, Ord, Show)

data KeyValue = KeyValue {
  kvCreateIndex :: Word64,
  kvLockIndex :: Word64,
  kvModifyIndex :: Word64,
  kvValue :: Maybe ByteString,
  kvFlags :: Word64,
  kvSession :: Maybe Text,
  kvKey :: Text
} deriving (Eq, Ord, Show)

data KeyValuePut = KeyValuePut {
  kvpKey :: Text,
  kvpValue :: ByteString,
  kvpCasIndex :: Maybe Word64,
  kvpFlags :: Maybe Word64
} deriving (Eq, Ord, Show)

data Session = Session {
  sId :: Text,
  sCreateIndex :: Maybe Word64
} deriving (Eq, Ord, Show)

data SessionInfo = SessionInfo {
  siId :: Text, -- TODO: switch to uuid
  siName :: Maybe Text,
  siNode :: Text,
  siLockDelay :: Maybe Word64,
  siBehavior :: Maybe SessionBehavior,
  siTtl :: Maybe Text,
  siChecks :: Maybe [Text],
  siNodeChecks :: Maybe [Text],
  siServiceChecks :: Maybe [Text],
  siCreateIndex :: Word64,
  siModifyIndex :: Word64
} deriving (Eq, Ord, Show)

newtype SessionInfoList = SessionInfoList [SessionInfo]

data SessionRequest = SessionRequest {
  srLockDelay :: Maybe Text,
  srName :: Maybe Text,
  srNode :: Node,
  srChecks :: [Text],
  srBehavor :: Maybe SessionBehavior,
  srTtl :: Maybe Text
} deriving (Eq, Ord, Show)

data Sequencer = Sequencer{
  sKey :: Text,
  sLockIndex :: Word64,
  sSession :: Session
} deriving (Eq, Ord, Show)

data RegisterRequest = RegisterRequest {
  rrDatacenter :: Maybe Datacenter,
  rrNode :: Text,
  rrAddress :: Text,
  rrService :: Maybe Service,
  rrCheck :: Maybe Check
} deriving (Eq, Ord, Show)

data Service = Service {
  seId :: Text,
  seService :: Text,
  seTags :: [Text],
  seAddress :: Maybe Text,
  sePort :: Maybe Int
} deriving (Eq, Ord, Show)

data ServiceResult = ServiceResult{
  srrNode :: Text,
  srrAddress :: Text,
  srrServiceId :: Text,
  srrServiceName :: Text,
  srrServiceTags :: [Text],
  srrServiceAddress :: Maybe Text,
  srrServicePort :: Maybe Int
} deriving (Eq, Ord, Show)

data Check = Check {
  cNode :: Text,
  cCheckId :: Text,
  cName :: Maybe Text,
  cNotes :: Maybe Text,
  cServiceId :: Maybe Text,
  cStatus :: HealthCheckStatus,
  cOutput :: Text,
  cServiceName :: Maybe Text
} deriving (Eq, Ord, Show)

data Node = Node {
  nNode :: Text,
  nAddress :: Text
} deriving (Eq, Ord, Show)

{- Agent -}
data RegisterHealthCheck = RegisterHealthCheck {
  rhcId :: Text,
  rhcName :: Text,
  rhcNotes :: Text,
  rhcScript :: Maybe Text,
  rhcInterval :: Maybe Text,
  rhcTtl :: Maybe Text
} deriving (Eq, Ord, Show)

data RegisterService = RegisterService {
  rsId :: Maybe Text,
  rsName :: Text,
  rsTags :: [Text],
  rsPort :: Maybe Int16,
  rsCheck :: Maybe HealthCheck
} deriving (Eq, Ord, Show)

data Self = Self {
  sMember :: Member
} deriving (Eq, Show)

data Config = Config {
  cBootstrap :: Bool,
  cServer :: Bool,
  cDatacenter :: Datacenter,
  cDataDir :: Text,
  cClientAddr :: Text
} deriving (Eq, Ord, Show)

data Member = Member {
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
} deriving (Eq, Show)

{- Health -}
data Health = Health {
  hNode :: Node,
  hService :: Service,
  hChecks :: [Check]
} deriving (Eq, Ord, Show)


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
  parseJSON (Object x) =
    Check
      <$> x .: "Node"
      <*> x .: "CheckID"
      <*> x .: "Name"
      <*> x .:? "Notes"
      <*> x .:? "ServiceID"
      <*> x .: "Status"
      <*> x .: "Output"
      <*> x .:? "ServiceName"
  parseJSON _ = mzero

instance FromJSON Service where
  parseJSON (Object x) =
    Service
      <$> x .: "ID"
      <*> x .: "Service"
      <*> x .: "Tags"
      <*> x .:? "Address"
      <*> x .:? "Port"
  parseJSON _ = mzero

instance FromJSON Node where
  parseJSON (Object x) =
    Node
      <$> x .: "Node"
      <*> x .: "Address"
  parseJSON _ = mzero

instance FromJSON Health where
  parseJSON (Object x) =
    Health
      <$> x.: "Node"
      <*> x .: "Service"
      <*> x .: "Checks"
  parseJSON _ = mzero

instance FromJSON Session where
  parseJSON (Object x) =
    Session
      <$> x .: "ID"
      <*> pure Nothing
  parseJSON _ = mzero

instance FromJSON SessionInfoList where
  parseJSON (Array x) =
    SessionInfoList
      <$> traverse parseJSON (toList x)
  parseJSON _ = mzero

instance FromJSON SessionInfo where
  parseJSON (Object x) =
    SessionInfo
      <$> x .:  "ID"
      <*> x .:? "Name"
      <*> x .:  "Node"
      <*> x .:? "LockDelay"
      <*> x .:? "Behavior"
      <*> x .:? "TTL"
      <*> x .:? "Checks"
      <*> x .:? "NodeChecks"
      <*> x .:? "ServiceChecks"
      <*> x .:  "CreateIndex"
      <*> x .:  "ModifyIndex"
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
  toJSON (SessionRequest lockDelay name node checks behavior ttl) = object["LockDelay" .= lockDelay, "Name" .= name, "Node" .= (nNode node), "Checks" .= checks, "Behavior" .= behavior, "TTL" .= ttl]

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
-- TODO: where to put these?
type ConsulHost = Text
type ApiEndpoint = Text
type ConsulQuery = Text
type ConsulRequestBody = ByteString
type WaitFlag = Bool

waitTrue = True
waitFalse = False
noQuery = Nothing
noRequestBody = Nothing
