{-# LANGUAGE TemplateHaskell #-}

module Network.Consul.TemplateHaskell (
    mkShim
  ) where

import Language.Haskell.TH
import Network.Consul.Types
import qualified Network.Consul.Internal as I

applyArgs :: ExpQ -> [ExpQ] -> ExpQ
applyArgs = foldl appE

mkShim :: [Name] -> Q [Dec]
mkShim fnNames = forM fnNames $ \fnName -> do
  -- shimmedName will be the name of the function used in Network.Consul
  let shimmedName = mkName $ nameBase fnName
  let clientE = varE $ mkName "_consul"
  let args = [appE (varE getter) clientE |
              getter <- ['ccManager, 'I.hostWithScheme, 'ccPort]]
  let body = normalB $ applyArgs (varE fnName) args
  let cls = clause [varP $ mkName "_consul"] body []
  dec <- funD shimmedName [cls]
  return dec
