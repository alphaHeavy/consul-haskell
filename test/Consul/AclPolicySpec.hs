{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.AclPolicySpec where

import Import

testsuiteSettingsWithAclsEnabled = 
  TestsuiteSettings
    { displayConsulServerLogs = True -- False -- 
    , enableAcls = True
    , verboseLogs = True -- False
    }


--createFailed :: String
createFailed e = expectationFailure ("AclPolicyCreate: failed " ++ e)

operationFailed testName e = expectationFailure (testName ++ ": failed " ++ e)

-- TODO: move to 
-- TODO: add type annotations
withEntity client@ConsulClient{..} apiFunction requestEntity errorFunction = do
  response <- apiFunction client requestEntity
  case response of
    Left e -> errorFunction e
    Right entity ->
      return entity
  

withOperation client@ConsulClient{..} apiFunction id errorFunction = do
  response <- apiFunction client id
  case response of
    Left e -> errorFunction e
    Right result -> return result


bootstrapAcls client testName consulPort = do
  response <- aclBootstrap client
  case response of
    Left e -> expectationFailure (testName ++ "(Bootstrap acls): failed " ++ e)
    Right aclBootstrapResponse -> do
      -- extract token from response
      let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
      return token

createPolicy client policyRequest testName =
  withEntity client aclPolicyCreate policyRequest (operationFailed testName)


spec :: Spec
spec = setupAround (consulServerSetupFuncWith testsuiteSettingsWithAclsEnabled) $ do

  it "AclPolicyCreate" $ \consulServerHandle -> do
    let consulHttpPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient $ consulHttpPort
    token <- bootstrapAcls client{ ccDatacenter = dc1 } "AclPolicyCreate" consulHttpPort
    -- create another consul client that is authenticated using the new token
    secondClient@ConsulClient{..} <- newClientAuth consulHttpPort token
    -- setup a policy record
    let policyName = "Test-Policy"
    let policyRequest =
          ConsulApiRequestAclPolicyCreate
            policyName
            "A policy for testing"
            "node_prefix \"\" { policy = \"read\"}"
            ["dc1"] -- None is all
    -- use the second (authenticated) client to create acl policy
    policy <- createPolicy secondClient policyRequest "AclPolicyCreate"
    context "AclPolicyCreate: successful" $ (aclPolicyName policy) `shouldBe` policyName


  it "AclPolicyDelete" $ \consulServerHandle -> do
    let consulHttpPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient $ consulHttpPort
    token <- bootstrapAcls client{ ccDatacenter = dc1 } "AclPolicyDelete" consulHttpPort
    -- create another consul client that is authenticated using the new token
    secondClient@ConsulClient{..} <- newClientAuth consulHttpPort token
    -- setup a policy record
    let policyName = "Test-Policy"
    let policyRequest =
          ConsulApiRequestAclPolicyCreate
            { consulApiRequestAclPolicyCreateName = policyName
            , consulApiRequestAclPolicyCreateDescription = "A policy for testing"
            , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
            , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
            }
    -- use the second (authenticated) client to create acl policy
    policy <- createPolicy secondClient policyRequest "AclPolicyDelete"
    deleteResult <- withOperation secondClient aclPolicyDelete (aclPolicyId policy) (operationFailed "AclPolicyDelete")
    case deleteResult of
      True -> context "AclPolicyDelete: successful!" $ pure ()
      False -> expectationFailure ("AclPolicyDelete: failed")


  it "AclPolicyUpdate" $ \consulServerHandle -> do
    let consulHttpPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient $ consulHttpPort
    token <- bootstrapAcls client{ ccDatacenter = dc1 } "AclPolicyUpdate" consulHttpPort
    -- create another consul client that is authenticated using the new token
    secondClient@ConsulClient{..} <- newClientAuth consulHttpPort token
    -- setup a policy record
    let policyName = "Test-Policy"
    let updatedPolicyName = "Updated-Policy-Name"
    let policyRequest =
          ConsulApiRequestAclPolicyCreate
            { consulApiRequestAclPolicyCreateName = policyName
            , consulApiRequestAclPolicyCreateDescription = "A policy for testing"
            , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
            , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
          --, consulApiRequestAclPolicyCreateNamespace = Nothing -- TODO: enterprise-only?
            }
    -- use the second (authenticated) client to create acl policy
    policy <- createPolicy secondClient policyRequest "AclPolicyUpdate"
    let updatedPolicyRequest =
          AclPolicy
            { aclPolicyId = (aclPolicyId policy)
            , aclPolicyName = updatedPolicyName 
            , aclPolicyDescription = (aclPolicyDescription policy)
            , aclPolicyRules = (aclPolicyRules policy)
            , aclPolicyDatacenters = (aclPolicyDatacenters policy)
            , aclPolicyHash = (aclPolicyHash policy)
            , aclPolicyCreateIndex = (aclPolicyCreateIndex policy)
            , aclPolicyModifyIndex = (aclPolicyModifyIndex policy)
            }
    updatedPolicy <- aclPolicyUpdate secondClient { ccDatacenter = dc1 } updatedPolicyRequest
    case updatedPolicy of
      Left e -> expectationFailure ("AclPolicyUpdate: updated policy failed " ++ e)
      Right updatedPolicyResponse -> do
        context "AclPolicyUpdate: successful" $ (aclPolicyName updatedPolicyResponse) `shouldBe` updatedPolicyName


  it "AclPolicyReadById" $ \consulServerHandle -> do
    let consulHttpPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient $ consulHttpPort
    token <- bootstrapAcls client{ ccDatacenter = dc1 } "AclPolicyReadById" consulHttpPort
    -- create another consul client that is authenticated using the new token
    secondClient@ConsulClient{..} <- newClientAuth consulHttpPort token
    -- setup a policy record
    let policyName = "Test-Policy"
    let policyRequest =
          ConsulApiRequestAclPolicyCreate
            { consulApiRequestAclPolicyCreateName = policyName
            , consulApiRequestAclPolicyCreateDescription = "A policy for testing"
            , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
            , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
          --, consulApiRequestAclPolicyCreateNamespace = Nothing -- TODO: enterprise-only?
            }
    -- use the second (authenticated) client to create acl policy
    policy <- createPolicy secondClient policyRequest "AclPolicyReadById"
    readItBack <- aclPolicyReadById (secondClient { ccDatacenter = dc1 }) (aclPolicyId policy)
    case readItBack of
      Left e -> expectationFailure ("AclPolicyReadById: failed " ++ e)
      Right p -> do
        context "AclPolicyReadById: successful" $ (aclPolicyName p) `shouldBe` policyName


  it "AclPolicyReadByName" $ \consulServerHandle -> do
    let consulHttpPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient $ consulHttpPort
    token <- bootstrapAcls client{ ccDatacenter = dc1 } "AclPolicyReadByName" consulHttpPort
    -- create another consul client that is authenticated using the new token
    secondClient@ConsulClient{..} <- newClientAuth consulHttpPort token
    -- setup a policy record
    let policyName = "Test-Policy"
    let policyRequest =
          ConsulApiRequestAclPolicyCreate
            { consulApiRequestAclPolicyCreateName = policyName
            , consulApiRequestAclPolicyCreateDescription = "A policy for testing"
            , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
            , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
          --, consulApiRequestAclPolicyCreateNamespace = Nothing -- TODO: enterprise-only?
            }
    -- use the second (authenticated) client to create acl policy
    policy <- createPolicy secondClient policyRequest "AclPolicyReadByName"
    readItBack <- aclPolicyReadByName (secondClient { ccDatacenter = dc1 }) (aclPolicyName policy)
    case readItBack of
      Left e -> expectationFailure ("AclPolicyReadById: failed " ++ e)
      Right p -> do
        context "AclPolicyReadById: successful" $ (aclPolicyName p) `shouldBe` policyName


  it "AclPolicyList" $ \consulServerHandle -> do
    let consulHttpPort = consulServerHandleHttpPort consulServerHandle
    client@ConsulClient{..} <- newClient $ consulHttpPort
    token <- bootstrapAcls client{ ccDatacenter = dc1 } "AclPolicyList" consulHttpPort
    -- create another consul client that is authenticated using the new token
    secondClient@ConsulClient{..} <- newClientAuth consulHttpPort token
    -- setup a policy record
    let policyName = "Test-Policy"
    let policyRequests =
          [ ConsulApiRequestAclPolicyCreate
              { consulApiRequestAclPolicyCreateName = policyName <> "-1"
              , consulApiRequestAclPolicyCreateDescription = "A policy for testing"
              , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
              , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
              }
          , ConsulApiRequestAclPolicyCreate
              { consulApiRequestAclPolicyCreateName = policyName <> "-2"
              , consulApiRequestAclPolicyCreateDescription = "A second policy for testing"
              , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
              , consulApiRequestAclPolicyCreateDatacenters = ["dc2"]
              }
          , ConsulApiRequestAclPolicyCreate
              { consulApiRequestAclPolicyCreateName = policyName <> "-3"
              , consulApiRequestAclPolicyCreateDescription = "A third policy for testing"
              , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"write\"}"
              , consulApiRequestAclPolicyCreateDatacenters = ["dc3"]
              }
          ]
    -- use the second (authenticated) client to create acl policy
    --policies <- map (\p -> aclPolicyCreate (secondClient { ccDatacenter = dc1 }) p) policyRequests 
    p1 <- createPolicy (secondClient { ccDatacenter = dc1 }) (policyRequests !! 0) "AclPolicyReadByName"
    p2 <- createPolicy (secondClient { ccDatacenter = dc1 }) (policyRequests !! 1) "AclPolicyReadByName"
    p3 <- createPolicy (secondClient { ccDatacenter = dc1 }) (policyRequests !! 2) "AclPolicyReadByName"
    policies <- aclPolicyList secondClient { ccDatacenter = dc1 }
    --print policies
    case policies of
      Left e -> expectationFailure ("AclPolicyList: failed " ++ e)
      Right aclPolicyList -> do
        --print aclPolicyList
        context "AclPolicyList: successful" $ pure ()
     -- map policies (\policy -> do
     --   case policy of
     --     Left e -> expectationFailure ("AclPolicyList: failed " ++ e)
     --     Right aclPolicyResponse -> do
     --       context "AclPolicyList: successful" $ (aclPolicyName aclPolicyResponse) `shouldBe` policyName)


