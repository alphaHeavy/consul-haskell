{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


{- | The functions in this module correspond to
the [Consul Namespaces API](https://www.consul.io/api/namespaces).

This module is a WIP, feel free to contribute via
the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).

__Missing Functions__

* `createNamespace`
* `deleteNamespace`
* `getNamespace`
* `listNamespaces`
* `updateNamespace`
-}
module Network.Consul.Client.Namespaces
  ( createNamespace
  , deleteNamespace
  , getNamespace     -- readNamespace
  , listNamespaces
  , updateNamespace
  ) where

import Import


{- | Create a new Namespace.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
createNamespace :: MonadIO m => ConsulClient -> SessionRequest -> m ()
createNamespace = undefined


{- | Delete a Namespace.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
deleteNamespace :: MonadIO m => ConsulClient -> SessionRequest -> m ()
deleteNamespace = undefined


{- | Get an existing Namespace.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
getNamespace :: MonadIO m => ConsulClient -> SessionRequest -> m ()
getNamespace = undefined


{- | List all Namespaces.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
listNamespaces :: MonadIO m => ConsulClient -> SessionRequest -> m ()
listNamespaces = undefined


{- | Update an existing Namespace.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
updateNamespace :: MonadIO m => ConsulClient -> SessionRequest -> m ()
updateNamespace = undefined
 
