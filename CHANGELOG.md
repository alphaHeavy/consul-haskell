# CHANGELOG

## v0.6.0

This release primarily moves the Datacenter into the ConsulClient data structure.
In effect, we're saying a "client connection" specifies the datacenter. So if you
want to interact with multiple datacenters in a consul cluster, you would have
multiple clients. We think this is a good thing, as MOST of the time, there's a
single datacenter we're targeting, and if/when there are multiple, having separate
clients is a great way to keep the code clean and obvious.

With this change, the datacenter doesn't need to be passed around outside the
client, we can simply rely on the client data structure to contain the info we
need for the interaction with consul.

With this update, users will need to change their calling code to use the library
(dropping the Maybe Datacenter from most functions, and specifying it when
initializing the client). An example of that update looks like:

```
+dc :: Datacenter
+dc = Datacenter "my-datacenter"
+
 newClient :: IO ConsulClient
-newClient = initializeConsulClient "localhost" consulPort emptyHttpManager
+newClient = initializeConsulClient "localhost" consulPort dc emptyHttpManager
```

```
 testGetInvalidKey :: TestTree
 testGetInvalidKey = testCase "testGetInvalidKey" $ do
   client@ConsulClient{..} <- newClient
-  x <- getKey client "nokey" Nothing Nothing Nothing
+  x <- getKey client "nokey" Nothing Nothing
   assertEqual "testGetInvalidKey: Found a key that doesn't exist" x Nothing
```

For more info, see [PR#33](https://github.com/alphaHeavy/consul-haskell/pull/33).

## v0.5.1

## v0.5.0
