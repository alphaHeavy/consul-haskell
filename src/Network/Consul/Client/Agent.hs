{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TODO: document module
module Network.Consul.Client.Agent
  ( getSelf
  ) where

import Import
import qualified Data.ByteString as B (concat) 
import qualified Data.ByteString.Lazy as BL (fromStrict)
import qualified Data.Text as T (concat, pack, unpack)

-- | TODO: Document

{- Agent -}
getSelf :: MonadIO m => ConsulClient -> m (Maybe Self)
getSelf _client@ConsulClient{..} =  do
  let hostnameWithScheme = hostWithScheme _client
  initReq <- liftIO $ parseUrlThrow $ T.unpack $ T.concat [hostnameWithScheme, ":", T.pack $ show ccPort ,"/v1/agent/self"]
  liftIO $ withResponse initReq ccManager $ \ response -> do
    bodyParts <- brConsume $ responseBody response
    let body = B.concat bodyParts
    return $ decode $ BL.fromStrict body


