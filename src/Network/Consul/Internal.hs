{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Consul.Internal
  ( hostWithScheme
  , createRequest 
  , decodeAndStrip
  , emptyHttpManager
  ) where

import Control.Monad.IO.Class
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Consul.Types
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Socket (PortNumber)

hostWithScheme :: ConsulClient -> Text
hostWithScheme ConsulClient{..} = scheme `T.append` ccHostname
  where scheme = if ccWithTls then "https://" else "http://"

createRequest :: MonadIO m => Text -> PortNumber -> Text -> Maybe Text -> Maybe ByteString -> Bool -> Maybe Datacenter -> m Request
createRequest hostWithScheme portNumber endpoint query body wait dc = do
  let baseUrl = T.concat [hostWithScheme,":",T.pack $ show portNumber,endpoint,needQueryString
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

decodeAndStrip :: ByteString -> String
decodeAndStrip = T.unpack . T.strip . TE.decodeUtf8


-- a convenience utility for clarity
emptyHttpManager :: Maybe Manager
emptyHttpManager = Nothing
