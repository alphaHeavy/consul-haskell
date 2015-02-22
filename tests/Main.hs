{-# LANGUAGE OverloadedStrings #-}
import qualified Network.Consul.Internal as I
import Network.HTTP.Client
import Network.Socket (PortNumber(..))
import Test.Tasty
import Test.Tasty.HUnit

manager = newManager defaultManagerSettings

{- Internal Tests -}
internalKVTests = testGroup "Internal Key Value" [testGetInvalidKey]

testGetInvalidKey = testCase "testGetInvalidKey" $ do
  man <- manager
  x <- I.getKey man "localhost" (PortNum 8500) "nokey" Nothing Nothing
  assertEqual "Found a key that doesn't exist" x Nothing

main :: IO ()
main = defaultMain internalKVTests
