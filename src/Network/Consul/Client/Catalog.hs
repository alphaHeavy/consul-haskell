{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
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
import qualified Data.HashMap.Strict as H (toList)

-- | TODO: Document
deregisterService :: MonadIO m => ConsulClient -> Text -> m ()
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


-- | TODO: document module
getDatacenters :: MonadIO m => ConsulClient -> m [Datacenter]
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


-- | TODO: Document
getService :: MonadIO m => ConsulClient -> Text -> Maybe Text -> m (Maybe [ServiceResult])
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


-- | TODO: Document
getServices :: MonadIO m => ConsulClient -> Maybe Text -> m [Text]
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
    parseServices t (Just (Object v)) = filterTags t $ H.toList v
    parseServices _   _               = []
    filterTags :: Maybe Text -> [(Text, Value)] -> [Text]
    filterTags (Just t)               = map fst . filter (\ (_, (Array v)) -> (String t) `V.elem` v)
    filterTags Nothing                = map fst


-- | TODO: Document
registerService :: MonadIO m => ConsulClient -> RegisterService -> m Bool
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

