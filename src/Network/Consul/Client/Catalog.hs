{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The functions in this module correspond to
the [Consul Catalog API](https://www.consul.io/api/catalog).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

__Missing Functions__

* `listServices` (synonym for `getServices`)
* `listNodes`
* `listDatacenters` (synonym for `getDatacenters`)
* `listNodesForService`
* `listNodesForConnectService`
* `getNodeServiceMap`
* `listNodeServices`
* `listGatewayServices`
-}
module Network.Consul.Client.Catalog
  ( deregisterService
  , getDatacenters
  , getService
  , getServices
  , registerService
  ) where

import Import
import qualified Data.ByteString as B (concat)
import qualified Data.ByteString.Lazy as BL (toStrict, fromStrict)
import qualified Data.Text as T (concat, pack, unpack)
import qualified Data.Vector as V (elem)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
#else
import qualified Data.HashMap.Strict as HashMap
#endif

{- |

TODO: Document.

@since 0.0.0.0
-}
deregisterService :: MonadIO m => ConsulClient -- ^
                  -> Text -- ^ Name of service to deregister.
                  -> m () -- ^
deregisterService client service = do
  let portNumber = ccPort client
      manager = ccManager client
      hostname = hostWithScheme client
      dc = ccDatacenter client
  initReq <- createRequest hostname
                           portNumber
                           (T.concat ["/v1/agent/service/deregister/", service])
                           Nothing
                           (Just "") -- forces PUT
                           False
                           dc
  liftIO $ withResponse initReq manager $ \ response -> do
    _bodyParts <- brConsume $ responseBody response
    return ()


{- |
TODO: Document

@since 0.1.0
-}
getDatacenters
  :: MonadIO m => ConsulClient -- ^
  -> m [Datacenter] -- ^
getDatacenters client@ConsulClient{..} = liftIO $ do
  let hostnameWithScheme = hostWithScheme client
  initReq <- parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/catalog/datacenters/"]
  withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let val = (decode $ BL.fromStrict body)
    case val of
      Just x -> return x
      Nothing -> return []


{- |

TODO: Document

@since 0.0.0.0
-}
getService
  :: MonadIO m => ConsulClient -- ^
  -> Text -- ^
  -> Maybe Text -- ^
  -> m (Maybe [ServiceResult]) -- ^
getService _client@ConsulClient{..} name tag = do
  let hostnameWithScheme = hostWithScheme _client
  req <- createRequest hostnameWithScheme
                       ccPort
                       (T.concat["/v1/catalog/service/",name])
                       (fmap (\ x -> T.concat ["tag=",x]) tag)
                       Nothing
                       False
                       ccDatacenter

  liftIO $ withResponse req ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    return $ decode $ BL.fromStrict $ B.concat bodyParts


{- |

TODO: Document

@since 0.0.0.0
-}
getServices
  :: MonadIO m => ConsulClient -- ^
  -> Maybe Text -- ^
  -> m [Text] -- ^
getServices _client@ConsulClient{..} tag = do
    req <- createRequest (hostWithScheme _client)
                         ccPort
                         "/v1/catalog/services"
                         Nothing
                         Nothing
                         False
                         ccDatacenter
    liftIO $ withResponse req ccManager $ \ response -> do
        bodyParts <- brConsume $ responseBody response
        return $ parseServices tag $ decode $ BL.fromStrict $ B.concat bodyParts
  where
    parseServices t (Just (Object v)) = filterTags t $
#if MIN_VERSION_aeson(2,0,0)
                                          map (\(k, va)-> (Aeson.Key.toText k, va)) $ Aeson.KeyMap.toList v
#else
                                          HashMap.toList v
#endif
    parseServices _   _               = []
    filterTags :: Maybe Text -> [(Text, Value)] -> [Text]
    filterTags (Just t)               = map fst . filter (\ (_, (Array v)) -> (String t) `V.elem` v)
    filterTags Nothing                = map fst


{- |

TODO: Document

@since 0.0.0.0
-}
registerService
  :: MonadIO m => ConsulClient -- ^
  -> RegisterService -- ^
  -> m Bool -- ^
registerService client request = do
  let portNumber = ccPort client
      manager = ccManager client
      hostname = hostWithScheme client
      dc = ccDatacenter client
  initReq <- createRequest hostname
                           portNumber
                           "/v1/agent/service/register"
                           Nothing
                           (Just $ BL.toStrict $ encode request)
                           False
                           dc
  liftIO $ withResponse initReq manager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> return True
      _ -> return False
