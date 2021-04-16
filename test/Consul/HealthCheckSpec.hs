{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Consul.HealthCheckSpec (spec) where

import Import
import Test.Syd

spec :: Spec
spec = do
  pure ()

-- spec = testGroup "Health Check Tests" [testGetServiceHealth]
-- 
-- {- Health Checks -}
-- 
-- {-
-- testRegisterHealthCheck :: TestTree
-- testRegisterHealthCheck = testCase "testRegisterHealthCheck" $ do
--   client@ConsulClient{..} <- newClient
--   let check = RegisterHealthCheck "testHealthCheck" "testHealthCheck" "" Nothing Nothing (Just "15s")
--   x1 <- registerHealthCheck ccManager (hostWithScheme client) ccPort check
--   undefined -}
-- 
-- testGetServiceHealth :: TestTree
-- testGetServiceHealth = testCase "testGetServiceHealth" $ do
--   client@ConsulClient{..} <- newClient
--   let req = RegisterService (Just "testGetServiceHealth") "testGetServiceHealth" [] Nothing Nothing
--   r1 <- registerService client req
--   case r1 of
--     True -> do
--       liftIO $ sleep 1
--       r2 <- getServiceHealth client "testGetServiceHealth"
--       case r2 of
--         Just [x] -> return ()
--         Just [] -> assertFailure "testGetServiceHealth: No Services Returned"
--         Nothing -> assertFailure "testGetServiceHealth: Failed to parse result"
--     False -> assertFailure "testGetServiceHealth: Service was not created"
