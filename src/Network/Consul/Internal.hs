{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TODO: Document
module Network.Consul.Internal
  ( hostWithScheme
  , createRequest 
  , decodeAndStrip
  , emptyHttpManager
  , parseTtl
  ) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Network.Consul.Types
import Network.HTTP.Client
import Network.HTTP.Types ()
import Network.Socket (PortNumber)

-- | TODO: Document
hostWithScheme :: ConsulClient -> ConsulHost
hostWithScheme ConsulClient{..} = scheme `T.append` ccHostname
  where scheme = if ccWithTls then "https://" else "http://"


-- TODO: document
createRequest :: MonadIO m => ConsulHost
                           -> PortNumber
                           -> ApiEndpoint
                           -> Maybe ConsulQuery
                           -> Maybe ConsulRequestBody
                           -> WaitFlag
                           -> Maybe Datacenter
                           -> m Request
createRequest consulHostWithScheme
              portNumber
              endpoint
              query
              body
              wait
              dc = do
  let baseUrl = T.concat [consulHostWithScheme,":",T.pack $ show portNumber,endpoint,needQueryString
                         ,maybe "" id query, prefixAnd, maybe "" (\ (Datacenter x) -> T.concat["dc=",x]) dc]
  initReq <- liftIO $ parseUrlThrow $ T.unpack baseUrl
  case body of
      Just x -> return $ indef $ initReq{ method = "PUT", requestBody = RequestBodyBS x, checkResponse = \ _ _ -> return ()}
      Nothing -> return $ indef $ initReq{checkResponse = \ _ _ -> return ()}
  where
    needQueryString = if isJust dc || isJust query then "?" else ""
    prefixAnd = if isJust query && isJust dc then "&" else ""
    indef req = if wait == True then req{responseTimeout = responseTimeoutNone} else req

{- Key Value Store -}

-- | TODO: Document
decodeAndStrip :: ByteString -> String
decodeAndStrip = T.unpack . T.strip . TE.decodeUtf8


-- | TODO: Document
-- a convenience utility for clarity
emptyHttpManager :: Maybe Manager
emptyHttpManager = Nothing


-- | TODO: Document
parseTtl :: Integral t => Text -> t
parseTtl ttl = let Right (x,_) = TR.decimal $ T.filter (/= 's') ttl in x
