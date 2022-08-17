{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
The contents of this module are internals used in the library.

Feel free to contribute via the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).
-}
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

{- |
Produces a formatted `ConsulHost` using values from the `ConsulClient` provided.

@since 0.0.0
-}
hostWithScheme :: ConsulClient -- ^
               -> ConsulHost -- ^ http://$hostname or https://$hostname, depends on `ccWithTls`.
hostWithScheme ConsulClient{..} = scheme `T.append` ccHostname
  where scheme = if ccWithTls then "https://" else "http://"


{- |
Format an Http `Request` we can send to Consul.

TODO: split up and clean up.

@since 0.0.0
-}
createRequest :: MonadIO m => ConsulHost -- ^
                           -> PortNumber -- ^
                           -> ApiEndpoint -- ^
                           -> Maybe ConsulQuery -- ^
                           -> Maybe ConsulRequestBody -- ^
                           -> WaitFlag -- ^
                           -> Maybe Datacenter -- ^
                           -> m Request -- ^
createRequest consulHostWithScheme
              portNumber
              endpoint
              query
              body
              wait
              dc = do
  let baseUrl = T.concat [consulHostWithScheme,":",T.pack $ show portNumber,endpoint,needQueryString
                         ,fromMaybe "" query, prefixAnd, maybe "" (\ (Datacenter x) -> T.concat["dc=",x]) dc]
  initReq <- liftIO $ parseUrlThrow $ T.unpack baseUrl
  case body of
      Just x -> return $ indef $ initReq{ method = "PUT", requestBody = RequestBodyBS x, checkResponse = \ _ _ -> return ()}
      Nothing -> return $ indef $ initReq{checkResponse = \ _ _ -> return ()}
  where
    needQueryString = if isJust dc || isJust query then "?" else ""
    prefixAnd = if isJust query && isJust dc then "&" else ""
    indef req = if wait then req{responseTimeout = responseTimeoutNone} else req

{- |
Run `T.unpack`, `T.strip` and `Text.Encoding.decoodeUtf8` on a `ByteString` (to get a `String` of course).

@since 0.0.0
-}
decodeAndStrip :: ByteString -- ^
               -> String -- ^
decodeAndStrip = T.unpack . T.strip . TE.decodeUtf8


{- |
A convenience utility for clarity.

@since 0.0.0
-}
emptyHttpManager :: Maybe Manager
emptyHttpManager = Nothing


{- |
Parse a TTL value.

TODO: review/update?

@since 0.0.0
-}
parseTtl :: Integral t => Text -- ^
         -> t -- ^
parseTtl ttl = let Right (x,_) = TR.decimal $ T.filter (/= 's') ttl in x
