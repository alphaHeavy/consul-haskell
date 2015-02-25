{-# LANGUAGE OverloadedStrings #-}
import Network.Consul.Types
import qualified Network.Consul.Internal as I
import Network.HTTP.Client
import Network.Socket (PortNumber(..))
import Test.Tasty
import Test.Tasty.HUnit

manager = newManager defaultManagerSettings

{- Internal Tests -}
internalKVTests = testGroup "Internal Key Value" [testGetInvalidKey, testPutKey]

testGetInvalidKey = testCase "testGetInvalidKey" $ do
  man <- manager
  x <- I.getKey man "localhost" (PortNum 8500) "nokey" Nothing Nothing
  assertEqual "Found a key that doesn't exist" x Nothing

testPutKey = testCase "testPutKey" $ do
  man <- manager
  let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
  x <- I.putKey man "localhost" (PortNum 8500) put Nothing
  print x
  return ()

main :: IO ()
main = defaultMain internalKVTests
