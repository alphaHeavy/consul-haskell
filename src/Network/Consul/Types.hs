{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Internal data type and instance definitions for interacting with Consul's API.

Please feel free to contribute via the
[repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).
-}
module Network.Consul.Types
  ( -- * Consul API Response Data Types
    Check(..)
  , Config(..)
  , Consistency(..)
  , ConsulClient(..)
  , Datacenter (..)
  , Health(..)
  , HealthCheck(..)
  , HealthCheckStatus(..)
  , Network.Consul.Types.KeyValue(..)
  , KeyValuePut(..)
  , Member(..)
  , Node(..)
  , RegisterRequest(..)
  , RegisterHealthCheck(..)
  , RegisterService(..)
  , Self(..)
  , Service(..)
  , ServiceResult(..)
  , Session(..)
  , SessionBehavior(..)
  , SessionInfo(..)
  , SessionRequest(..)
  , Sequencer(..)
  , WANCoordinates(..)
  , NodeCoordinates(..)
  , ConsulHost
    -- * Internal Data Types
  , ApiEndpoint
  , ConsulQuery
  , ConsulRequestBody
  , WaitFlag
    -- * Utility Data Types
  , noQuery
  , noRequestBody
  , waitTrue
  , waitFalse
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Foldable
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word
import Network.HTTP.Client (Manager)
import Network.Socket


{- | Represents a Consul Client.

@since 0.0.0.0 
-}
data ConsulClient = ConsulClient
  { ccManager :: Manager -- ^ Http Manager
  , ccHostname :: Text  -- ^ Host/Ip to Consul agent
  , ccPort :: PortNumber -- ^ Tcp Port to Http interface on Consul agent
  , ccWithTls :: Bool -- ^ Do we enable Tls on the connection?
  , ccDatacenter :: Maybe Datacenter -- ^ Is there a datacenter we scope our requests to?
  }


{- | Represents a Consul Datacenter.

@since 0.1.0
-}
data Datacenter = Datacenter Text deriving (Eq, Ord, Show)


{- | Represents Consul's documented Consistency Modes.

See [Consul's doocumentation](https://www.consul.io/api-docs/features/consistency) for more info.

@since 0.1.0
-}
data Consistency
  = Consistent -- ^ This mode is strongly consistent without caveats.
  | Default -- ^ If not specified, the default is strongly consistent in almost all cases.
  | Stale -- ^ This mode allows any server to service the read, even if it is not the active leader.
  deriving (Bounded, Enum, Eq, Ord, Show)


{- | Represents Consul's Health Check Status

@since 0.1.0
-}
data HealthCheckStatus
  = Critical -- ^ Health check is in a critical state.
  | Passing -- ^ Health check is passing.
  | Unknown -- ^ Status of health check is unknown.
  | Warning -- ^ Health check is warning (not passing, but not critical yet either).
  deriving (Bounded, Enum, Eq, Ord, Show)


{- | Represents Consul's possible Session Behavior

@since 0.0.0.0
-}
data SessionBehavior
  = Release -- ^ Causes any locks that are held to be released.
  | Delete -- ^ Causes any locks that are held to be deleted.
  deriving (Bounded, Enum, Eq, Ord, Show)


{- | Represents Consul's Health Check

@since 0.0.0.0
-}
data HealthCheck
   = Script Text Text -- ^ TODO
   | Ttl Text -- ^ TODO
   | Http Text -- ^ TODO
   deriving (Eq, Ord, Show)


{- | Represents a Consul Key retrieved from the KV store.

@since 0.0.0.0
-}
data KeyValue = KeyValue
  { kvCreateIndex :: Word64
  , kvLockIndex :: Word64
  , kvModifyIndex :: Word64
  , kvValue :: Maybe ByteString
  , kvFlags :: Word64
  , kvSession :: Maybe Text
  , kvKey :: Text
  } deriving (Eq, Ord, Show)


{- | Represents a KV being written (PUT) to the Consul KV.

@since 0.1.0
-}
data KeyValuePut = KeyValuePut
  { kvpKey :: Text
  , kvpValue :: ByteString
  , kvpCasIndex :: Maybe Word64
  , kvpFlags :: Maybe Word64
  } deriving (Eq, Ord, Show)


{- | Represents a Consul Session

@since 0.1.0
-}
data Session = Session
  { sId :: Text
  , sCreateIndex :: Maybe Word64
  } deriving (Eq, Ord, Show)


{- | Represents the session info/data associated with a session.

@since 0.0.0.0
-}
data SessionInfo = SessionInfo
  { siId :: Text -- TODO: switch to uuid
  , siName :: Maybe Text
  , siNode :: Text
  , siLockDelay :: Maybe Word64
  , siBehavior :: Maybe SessionBehavior
  , siTtl :: Maybe Text
  , siChecks :: Maybe [Text]
  , siNodeChecks :: Maybe [Text]
  , siServiceChecks :: Maybe [Text]
  , siCreateIndex :: Word64
  , siModifyIndex :: Word64
  } deriving (Eq, Ord, Show)


{- |

@since 0.0.0.0
-}
newtype SessionInfoList
  = SessionInfoList [SessionInfo] -- ^ List of `SessionInfo` objects.


{- |

@since 0.0.0.0
-}
data SessionRequest = SessionRequest
  { srLockDelay :: Maybe Text
  , srName :: Maybe Text
  , srNode :: Node
  , srChecks :: [Text]
  , srBehavor :: Maybe SessionBehavior
  , srTtl :: Maybe Text
  } deriving (Eq, Ord, Show)


{- |

@since 0.0.0.0
-}
data Sequencer = Sequencer
  { sKey :: Text
  , sLockIndex :: Word64
  , sSession :: Session
  } deriving (Eq, Ord, Show)


{- |

@since 0.1.0
-}
data RegisterRequest = RegisterRequest
  { rrDatacenter :: Maybe Datacenter
  , rrNode :: Text
  , rrAddress :: Text
  , rrService :: Maybe Service
  , rrCheck :: Maybe Check
  } deriving (Eq, Ord, Show)


{- |

@since 0.1.0
-}
data Service = Service
  { seId :: Text
  , seService :: Text
  , seTags :: [Text]
  , seAddress :: Maybe Text
  , sePort :: Maybe Int
  } deriving (Eq, Ord, Show)


{-

@since 0.0.0.0
-}
data ServiceResult = ServiceResult
  { srrNode :: Text
  , srrAddress :: Text
  , srrServiceId :: Text
  , srrServiceName :: Text
  , srrServiceTags :: [Text]
  , srrServiceAddress :: Maybe Text
  , srrServicePort :: Maybe Int
  } deriving (Eq, Ord, Show)


{- |

@since 0.1.0
-}
data Check = Check
  { cNode :: Text
  , cCheckId :: Text
  , cName :: Maybe Text
  , cNotes :: Maybe Text
  , cServiceId :: Maybe Text
  , cStatus :: HealthCheckStatus
  , cOutput :: Text
  , cServiceName :: Maybe Text
  } deriving (Eq, Ord, Show)


{- |

@since 0.2.0
-}
data Node = Node
  { nNode :: Text
  , nAddress :: Text
  } deriving (Eq, Ord, Show)


{-| Agent 

@since 0.0.0.0
-}
data RegisterHealthCheck = RegisterHealthCheck
  { rhcId :: Text
  , rhcName :: Text
  , rhcNotes :: Text
  , rhcScript :: Maybe Text
  , rhcInterval :: Maybe Text
  , rhcTtl :: Maybe Text
  } deriving (Eq, Ord, Show)


{- |

@since 0.0.0.0
-}
data RegisterService = RegisterService
  { rsId :: Maybe Text
  , rsName :: Text
  , rsTags :: [Text]
  , rsPort :: Maybe Int16
  , rsCheck :: Maybe HealthCheck
  } deriving (Eq, Ord, Show)


{- |

@since 0.0.0.0
-}
data Self = Self
  { sMember :: Member
  } deriving (Eq, Show)


{- |

@since 0.0.0.0
-}
data Config = Config
  { cBootstrap :: Bool
  , cServer :: Bool
  , cDatacenter :: Datacenter
  , cDataDir :: Text
  , cClientAddr :: Text
  } deriving (Eq, Ord, Show)


{- |

@since 0.0.0.0
-}
data Member = Member
  { mName :: Text
  , mAddress :: Text
  , mPort :: Int 
  , mTags :: Object
  , mStatus :: Int
  , mProtocolMin :: Int
  , mProtocolMax :: Int
  , mProtocolCur :: Int
  , mDelegateMin :: Int
  , mDelegateMax :: Int
  , mDelegateCur :: Int
  } deriving (Eq, Show)


{- |

Health

@since 0.0.0.0
-}
data Health = Health
  { hNode :: Node
  , hService :: Service
  , hChecks :: [Check]
  } deriving (Eq, Ord, Show)


{- | JSON Instances

@since 0.0.0.0
-}
instance FromJSON Self where
  parseJSON (Object v) =
    Self
      <$> v .: "Member"
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON Config where
  parseJSON (Object v) =
    Config
      <$> v .: "Bootstrap"
      <*> v .: "Server"
      <*> v .: "Datacenter"
      <*> v .: "DataDir"
      <*> v .: "ClientAddr"
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON Member where
  parseJSON (Object v) =
    Member
      <$> v .: "Name"
      <*> v .: "Addr"
      <*> v .: "Port"
      <*> v .: "Tags"
      <*> v .: "Status"
      <*> v .: "ProtocolMin"
      <*> v .: "ProtocolMax"
      <*> v .: "ProtocolCur"
      <*> v .: "DelegateMin"
      <*> v .: "DelegateMax"
      <*> v .: "DelegateCur"
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON HealthCheckStatus where
  parseJSON (String "critical") = pure Critical
  parseJSON (String "passing") = pure Passing
  parseJSON (String "unknown") = pure Unknown
  parseJSON (String "warning") = pure Warning
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
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


{- |

@since 0.0.0.0
-}
instance FromJSON Datacenter where
  parseJSON (String val) = pure $ Datacenter val
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON Check where
  parseJSON (Object x) =
    Check
      <$> x .:  "Node"
      <*> x .:  "CheckID"
      <*> x .:  "Name"
      <*> x .:? "Notes"
      <*> x .:? "ServiceID"
      <*> x .:  "Status"
      <*> x .:  "Output"
      <*> x .:? "ServiceName"
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON Service where
  parseJSON (Object x) =
    Service
      <$> x .:  "ID"
      <*> x .:  "Service"
      <*> x .:  "Tags"
      <*> x .:? "Address"
      <*> x .:? "Port"
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON Node where
  parseJSON (Object x) =
    Node
      <$> x .: "Node"
      <*> x .: "Address"
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON Health where
  parseJSON (Object x) =
    Health
      <$> x .: "Node"
      <*> x .: "Service"
      <*> x .: "Checks"
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON Session where
  parseJSON (Object x) =
    Session
      <$> x .: "ID"
      <*> pure Nothing
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance FromJSON SessionInfoList where
  parseJSON (Array x) =
    SessionInfoList
      <$> traverse parseJSON (toList x)
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
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


{- |

@since 0.0.0.0
-}
instance FromJSON SessionBehavior where
  parseJSON (String "release") = pure Release
  parseJSON (String "delete") = pure Delete
  parseJSON _ = mzero


{- |

@since 0.0.0.0
-}
instance ToJSON SessionBehavior where
  toJSON Release = String "release"
  toJSON Delete = String "delete"


{- |

@since 0.0.0.0
-}
instance ToJSON RegisterHealthCheck where
  toJSON (RegisterHealthCheck i name notes script interval ttl) =
    object
      [ "id" .= i
      , "name" .= name
      , "notes" .= notes
      , "script" .= script
      , "interval" .= interval
      , "ttl" .= ttl
      ]


{- |

@since 0.0.0.0
-}
instance ToJSON RegisterService where
  toJSON (RegisterService i name tags port check) =
    object
      [ "ID" .= i
      , "Name" .= name
      , "tags" .= tags
      , "port" .= port
      , "Check" .= check
      ]


{- |

@since 0.0.0.0
-}
instance ToJSON HealthCheck where
  toJSON (Ttl x) = object ["TTL" .= x]
  toJSON (Http x) = object ["HTTP" .= x]
  toJSON (Script x y) = object ["Script" .= x, "Interval" .= y]


{- |

@since 0.0.0.0
-}
instance ToJSON SessionRequest where
  toJSON (SessionRequest lockDelay name node checks behavior ttl) =
    object
      [ "LockDelay" .= lockDelay
      , "Name" .= name
      , "Node" .= (nNode node)
      , "Checks" .= checks
      , "Behavior" .= behavior
      , "TTL" .= ttl
      ]


{- |

@since 0.0.0.0
-}
instance ToJSON ServiceResult where
  toJSON (ServiceResult node addr sid sName sTags sAddress sPort) =
    object
      [ "Node" .= node
      , "Address" .= addr
      , "ServiceID" .= sid
      , "ServiceName" .= sName
      , "ServiceTags" .= sTags
      , "ServiceAddress" .= sAddress
      , "ServicePort" .= sPort
      ]


{- |

@since 0.0.0.0
-}
instance FromJSON ServiceResult where
  parseJSON (Object x) =
    ServiceResult
      <$> x .:  "Node"
      <*> x .:  "Address"
      <*> x .:  "ServiceID"
      <*> x .:  "ServiceName"
      <*> x .:  "ServiceTags"
      <*> x .:? "ServiceAddress"
      <*> x .:? "ServicePort"
  parseJSON _ = mzero

{- |

@since 0.0.0.0
-}
data WANCoordinates = WANCoordinates
  { datacenter :: Datacenter
  , areaID :: Maybe Text
  , coordinates :: [NodeCoordinates]
  } deriving (Eq, Show)

{- |

@since 0.0.0.0
-}
instance FromJSON WANCoordinates where
  parseJSON (Object v) =
    WANCoordinates
      <$> v .: "Datacenter"
      <*> v .: "AreaID"
      <*> v .: "Coordinates"
  parseJSON _ = mzero

-- TODO: document
data NodeCoordinates = NodeCoordinates
  { node :: Text
  , segment :: Maybe Text
  , coord :: Coordinate
  } deriving (Eq, Show)

{- |

@since 0.0.0.0
-}
instance FromJSON NodeCoordinates where
  parseJSON (Object v) =
    NodeCoordinates
      <$> v .:  "Node"
      <*> v .:? "Segment"
      <*> v .:  "Coord"
  parseJSON _ = mzero

-- | TODO: check that this is correct, what to do about Maybe segment?
instance ToJSON NodeCoordinates where
  toJSON (NodeCoordinates node segment coord) =
    object
      [ "Node" .= node
      , "Segment" .= segment
      , "Coordinate" .= coord
      ]

data Coordinate = Coordinate
  { adjustment :: Int
  , error :: Double
  , height :: Int
  , vec :: [Int]
  } deriving (Eq, Show)


{- |

@since 0.0.0.0
-}
instance FromJSON Coordinate where
  parseJSON (Object v) =
    Coordinate
      <$> v .: "Adjustment"
      <*> v .: "Error"
      <*> v .: "Height"
      <*> v .: "Vec"
  parseJSON _ = mzero

-- | TODO: check that this is correct, what to do about [Int] vec? make a Vec data?
instance ToJSON Coordinate where
  toJSON (Coordinate adjustment error height vec) =
    object
      [ "Adjustment" .= adjustment
      , "Error" .= error
      , "Height" .= height
      , "Vec" .= vec
      ]


-- Internal Types

{- | TODO: Review and doocument

@since 0.0.0.0
-}
foo :: Maybe Value -> Parser (Maybe ByteString)
foo (Just (String x)) =
  case B64.decode $ TE.encodeUtf8 x of
    Left y -> fail y
    Right y -> return $ Just y
foo (Just _) = return Nothing
foo Nothing = return Nothing


{- | Represents the Hostname/IP for the Consul Server to interact with.

@since 0.0.0.0
-}
type ConsulHost = Text


{- | Represents the Url path for the API the request is interacting with.

@since 0.0.0.0
-}
type ApiEndpoint = Text


{- | Represents a query to include in our Consul Request.

@since 0.0.0.0
-}
type ConsulQuery = Text


{- | Represents the body of the request sent to Consul.

@since 0.0.0.0
-}
type ConsulRequestBody = ByteString


{- | Whether or not wait is enabled on the Consul request.

@since 0.0.0.0
-}
type WaitFlag = Bool


{- | Wait is enabled.

@since 0.0.0.0
-}
waitTrue :: Bool
waitTrue = True


{- | Wait is disabled.

@since 0.0.0.0
-}
waitFalse :: Bool
waitFalse = False


{- | Represents the case when we have no query to include in the Consul request.

@since 0.0.0.0
-}
noQuery :: Maybe a
noQuery = Nothing


{- | Represents the case when we have no data payload (body) to include in the Consul request.

@since 0.0.0.0
-}
noRequestBody :: Maybe a
noRequestBody = Nothing
