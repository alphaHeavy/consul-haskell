{-# LANGUAGE OverloadedStrings #-}
import Network.Consul.Types
import qualified Network.Consul.Internal as I
import Network.HTTP.Client
import Network.Socket (PortNumber(..))
import Test.Tasty
import Test.Tasty.HUnit

manager :: IO Manager
manager = newManager defaultManagerSettings

{- Internal Tests -}
internalKVTests :: TestTree
internalKVTests = testGroup "Internal Key Value" [testGetInvalidKey, testPutKey, 
  testGetKey,testGetKeys,testListKeys,testDeleteKey]

testGetInvalidKey :: TestTree
testGetInvalidKey = testCase "testGetInvalidKey" $ do
  man <- manager
  x <- I.getKey man "localhost" (PortNum 8500) "nokey" Nothing Nothing Nothing
  assertEqual "testGetInvalidKey: Found a key that doesn't exist" x Nothing

testPutKey :: TestTree
testPutKey = testCase "testPutKey" $ do
  man <- manager
  let put = KeyValuePut "/testPutKey" "Test" Nothing Nothing
  x <- I.putKey man "localhost" (PortNum 8500) put Nothing
  assertEqual "testPutKey: Write failed" True x

testGetKey :: TestTree
testGetKey = testCase "testGetKey" $ do
  man <- manager
  let put = KeyValuePut "/testGetKey" "Test" Nothing Nothing
  x1 <- I.putKey man "localhost" (PortNum 8500) put Nothing
  assertEqual "testGetKey: Write failed" True x1
  x2 <- I.getKey man "localhost" (PortNum 8500) "/testGetKey" Nothing Nothing Nothing
  case x2 of
    Just x -> assertEqual "testGetKey: Incorrect Value" (kvValue x) "Test"
    Nothing -> assertFailure "testGetKey: No value returned"

testGetKeys :: TestTree
testGetKeys = testCase "testGetKeys" $ do
  man <- manager
  let put1 = KeyValuePut "/testGetKeys/key1" "Test" Nothing Nothing
  x1 <- I.putKey man "localhost" (PortNum 8500) put1 Nothing
  assertEqual "testGetKeys: Write failed" True x1
  let put2 = KeyValuePut "/testGetKeys/key2" "Test" Nothing Nothing
  x2 <- I.putKey man "localhost" (PortNum 8500) put2 Nothing
  assertEqual "testGetKeys: Write failed" True x2
  x3 <- I.getKeys man "localhost" (PortNum 8500) "/testGetKeys" Nothing Nothing Nothing
  assertEqual "testGetKeys: Incorrect number of results" 2 (length x3)

testListKeys :: TestTree
testListKeys = testCase "testListKeys" $ do
  man  <- manager
  let put1 = KeyValuePut "/testListKeys/key1" "Test" Nothing Nothing
  x1 <- I.putKey man "localhost" (PortNum 8500) put1 Nothing
  assertEqual "testListKeys: Write failed" True x1
  let put2 = KeyValuePut "/testListKeys/key2" "Test" Nothing Nothing
  x2 <- I.putKey man "localhost" (PortNum 8500) put2 Nothing
  assertEqual "testListKeys: Write failed" True x2
  x3 <- I.listKeys man "localhost" (PortNum 8500) "/testListKeys/" Nothing Nothing Nothing
  assertEqual "testListKeys: Incorrect number of results" 2 (length x3)

testDeleteKey :: TestTree
testDeleteKey = testCase "testDeleteKey" $ do
  man  <- manager
  let put1 = KeyValuePut "/testDeleteKey" "Test" Nothing Nothing
  x1 <- I.putKey man "localhost" (PortNum 8500) put1 Nothing
  assertEqual "testDeleteKey: Write failed" True x1
  I.deleteKey man "localhost" (PortNum 8500) "/testDeleteKey" False Nothing
  x2 <- I.getKey man "localhost" (PortNum 8500) "/testDeleteKey" Nothing Nothing Nothing
  assertEqual "testDeleteKey: Key was not deleted" Nothing x2

testRegisterHealthCheck :: TestTree
testRegisterHealthCheck = testCase "testRegisterHealthCheck" $ do
  man <- manager
  let check = RegisterHealthCheck "testHealthCheck" "testHealthCheck" "" Nothing Nothing (Just "15s")
  x1 <- I.registerHealthCheck man "localhost" (PortNum 8500) check
  undefined

main :: IO ()
main = defaultMain internalKVTests
