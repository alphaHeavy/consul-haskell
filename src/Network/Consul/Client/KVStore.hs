{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


{- | __KVStore Client API__

The functions in this module correspond to
the [Consul KV API](https://www.consul.io/api/kv).

__Missing Functions__

* TODO: review.
-}
module Network.Consul.Client.KVStore
  ( deleteKey
  , getKey
  , getKeys
  , listKeys
  , putKey
  , putKeyAcquireLock
  , putKeyReleaseLock
  ) where

import Import
import qualified Data.ByteString as B (concat) 
import qualified Data.ByteString.Lazy as BL (fromStrict)
import qualified Data.Text as T (concat, empty, pack, intercalate)
import Network.Consul.Types (KeyPath(..))
 
{- | Delete Key

TODO: Document.

@since 0.0.0.0
-}
deleteKey :: MonadIO m => ConsulClient -- ^
          -> Text -- ^
          -> Bool -- ^
          -> m Bool -- ^
deleteKey _client@ConsulClient{..} key recurse = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", key])
                           (if recurse then Just "recurse" else Nothing)
                           Nothing
                           False
                           ccDatacenter
  let httpReq = initReq { method = "DELETE"}
  liftIO $ withResponse httpReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let result = decodeAndStrip body
    case result of
      "true" -> return True
      "false" -> return False
      _ -> return False


{- |
TODO: Document

@since 0.1.0
-}
getKey
  :: MonadIO m => ConsulClient -- ^
  -> KeyPath -- ^
  -> Maybe Word64 -- ^
  -> Maybe Consistency -- ^
  -> m (Maybe KeyValue) -- ^
getKey _client@ConsulClient{..} key index consistency = do
  let hostnameWithScheme = hostWithScheme _client
  request <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", unKeyPath key])
                           fquery
                           Nothing
                           (isJust index)
                           ccDatacenter
  liftIO $ withResponse request ccManager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> do
        bodyParts <- brConsume $ responseBody response
        let body = B.concat bodyParts
        return $ listToMaybe =<< decode (BL.fromStrict body)
      _ -> return Nothing
  where
    cons = fmap (\ x -> T.concat["consistency=", T.pack $ show x] ) consistency
    ind = fmap (\ x -> T.concat["index=", T.pack $ show x]) index
    query = T.intercalate "&" $ catMaybes [cons,ind]
    fquery = if query /= T.empty then Just query else Nothing


{- | Get Keys

TODO: Document.

@since 0.0.0.0
-}
getKeys
  :: MonadIO m => ConsulClient -- ^
  -> Text -- ^
  -> Maybe Word64 -- ^
  -> Maybe Consistency -- ^
  -> m [KeyValue] -- ^
getKeys _client@ConsulClient{..} key index consistency = do
  let hostnameWithScheme = hostWithScheme _client
  request <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/",key])
                           fquery
                           Nothing
                           (isJust index)
                           ccDatacenter
  liftIO $ withResponse request ccManager $ \ response -> do
    case responseStatus response of
      x | x == status200 -> do
        bodyParts <- brConsume $ responseBody response
        let body = B.concat bodyParts
        return $ maybe [] id $ decode $ BL.fromStrict body
      _ -> return []
  where
    cons = fmap (\ x -> T.concat["consistency=", T.pack $ show x] ) consistency
    ind = fmap (\ x -> T.concat["index=", T.pack $ show x]) index
    query = T.intercalate "&" $ catMaybes [cons,ind, Just "recurse"]
    fquery = if query /= T.empty then Just query else Nothing


{- | List Keys.

TODO: Document.

@since 0.0.0.0
-}
listKeys
  :: MonadIO m => ConsulClient -- ^
  -> Text -- ^
  -> Maybe Word64 -- ^
  -> Maybe Consistency -- ^
  -> m [Text] -- ^
listKeys _client@ConsulClient{..} prefix index consistency = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", prefix])
                           fquery
                           Nothing
                           (isJust index)
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response ->
    case responseStatus response of
      x | x == status200 -> do
        bodyParts <- brConsume $ responseBody response
        let body = B.concat bodyParts
        return $ maybe [] id $ decode $ BL.fromStrict body
      _ -> return []
  where
    cons = fmap (\ x -> T.concat["consistency=", T.pack $ show x] ) consistency
    ind = fmap (\ x -> T.concat["index=", T.pack $ show x]) index
    query = T.intercalate "&" $ catMaybes [cons,ind, Just "keys"]
    fquery = if query /= T.empty then Just query else Nothing


{- |
TODO: Document

@since 0.1.0
-}
putKey
  :: MonadIO m => ConsulClient -- ^
  -> KeyValuePut -- ^
  -> m Bool -- ^
putKey _client@ConsulClient{..} putRequest = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", unKeyPath $ kvpKey putRequest])
                           fquery
                           (Just $ kvpValue putRequest)
                           False
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let result = decodeAndStrip body
    case result of
      "true" -> return True
      "false" -> return False
      _ -> return False
  where
    flags = fmap (\ x -> T.concat["flags=", T.pack $ show x]) $ kvpFlags putRequest
    cas = fmap (\ x -> T.concat["cas=",T.pack $ show x]) $ kvpCasIndex putRequest
    query = T.intercalate "&" $ catMaybes [flags,cas]
    fquery = if query /= T.empty then Just query else Nothing


{- | Put Key Acquire Lock

TODO: Document.

@since 0.0.0.0
-}
putKeyAcquireLock
  :: MonadIO m => ConsulClient -- ^
  -> KeyValuePut -- ^
  -> Session -- ^
  -> m Bool -- ^
putKeyAcquireLock _client@ConsulClient{..} request (Session session _) = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", unKeyPath $ kvpKey request])
                           fquery
                           (Just $ kvpValue request)
                           False
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let result = decodeAndStrip body
    case result of
      "true" -> return True
      "false" -> return False
      _ -> return False
  where
    flags = fmap (\ x -> T.concat["flags=", T.pack $ show x]) $ kvpFlags request
    cas = fmap (\ x -> T.concat["cas=",T.pack $ show x]) $ kvpCasIndex request
    acquire = T.concat["acquire=",session]
    query = T.intercalate "&" $ catMaybes [flags,cas,Just acquire]
    fquery = if query /= T.empty then Just query else Nothing


{- | Put Key Release Lock

TODO: Document.

@since 0.0.0.0
-}
putKeyReleaseLock
  :: MonadIO m => ConsulClient -- ^
  -> KeyValuePut -- ^
  -> Session -- ^
  -> m Bool -- ^
putKeyReleaseLock _client@ConsulClient{..} request (Session session _) = do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- createRequest hostnameWithScheme
                           ccPort
                           (T.concat ["/v1/kv/", unKeyPath $ kvpKey request])
                           fquery
                           (Just $ kvpValue request)
                           False
                           ccDatacenter
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    let result = decodeAndStrip body
    case result of
      "true" -> return True
      "false" -> return False
      _ -> return False
  where
    flags = fmap (\ x -> T.concat["flags=", T.pack $ show x]) $ kvpFlags request
    cas = fmap (\ x -> T.concat["cas=",T.pack $ show x]) $ kvpCasIndex request
    release = T.concat["release=",session]
    query = T.intercalate "&" $ catMaybes [flags,cas,Just release]
    fquery = if query /= T.empty then Just query else Nothing

