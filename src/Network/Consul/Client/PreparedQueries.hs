{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The functions in this module correspond to
the [Consul Prepared Queries API](https://www.consul.io/api/query).

__Missing Functions__

* `createQuery`
* `deleteQuery`
* `executeQuery`
* `explainQuery`
* `getQuery`
* `listQueries`
* `updateQuery`

Feel free to contribute via the [repo on GitHub](https://github.com/AlphaHeavy/consul-haskell).
-}
module Network.Consul.Client.PreparedQueries
  ( createQuery
  , deleteQuery
  , executeQuery
  , explainQuery
  , getQuery
  , listQueries
  , updateQuery
  ) where

import Import

{- | Create a new Prepared Query.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
createQuery :: MonadIO m => ConsulClient -> SessionRequest -> m ()
createQuery = undefined


{- | Delete a Prepared Query.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
deleteQuery :: MonadIO m => ConsulClient -> SessionRequest -> m ()
deleteQuery = undefined



{- | Execute a Prepared Query (return the results from the Query).

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
executeQuery :: MonadIO m => ConsulClient -> m ()
executeQuery = undefined


{- | Explain a Prepared Query.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
explainQuery :: MonadIO m => ConsulClient -> m ()
explainQuery = undefined


{- | Get an existing Prepared Query.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
getQuery :: MonadIO m => ConsulClient -> m ()
getQuery = undefined


{- | List existing Prepared Queries.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
listQueries :: MonadIO m => ConsulClient -> m ()
listQueries = undefined


{- | Update a Prepared Query.

Undefined, please feel free to contribute a solution for this missing function.

@since 0.0.0.0
-}
updateQuery :: MonadIO m => ConsulClient -> m ()
updateQuery = undefined
