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

spec :: Spec
spec = setupAround (consulServerSetupFuncWith testsuiteSettingsWithAclsEnabled) $ do

  it "AclPolicyCreate" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclPolicyCreate(Bootstrap acls): failed " ++ e)
      Right aclBootstrapResponse -> do
        -- extract token from response
        let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
        -- create another consul client that is authenticated using the new token
        secondClient@ConsulClient{..} <-
          newClientAuth
            (consulServerHandleHttpPort consulServerHandle)
            token
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
        policy <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case policy of
          Left e -> expectationFailure ("AclPolicyCreate: failed " ++ e)
          Right aclPolicyResponse -> do
            context "AclPolicyCreate: successful" $ (aclPolicyName aclPolicyResponse) `shouldBe` policyName

  it "AclPolicyDelete" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclPolicyCreate(Bootstrap acls): failed " ++ e)
      Right aclBootstrapResponse -> do
        -- extract token from response
        let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
        -- create another consul client that is authenticated using the new token
        secondClient@ConsulClient{..} <-
          newClientAuth
            (consulServerHandleHttpPort consulServerHandle)
            token
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
        policy <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case policy of
          Left e -> expectationFailure ("AclPolicyDelete: failed " ++ e)
          Right aclPolicyResponse -> do
            deleteResult <- liftIO $ aclPolicyDelete secondClient (aclPolicyId aclPolicyResponse)
            case deleteResult of
              Left e -> expectationFailure ("AclPolicyDelete: failed! " ++ e)
              Right result ->
                case result of
                  True -> context "AclPolicyDelete: successful!" $ pure ()
                  False -> expectationFailure ("AclPolicyDelete: failed")


  it "AclPolicyUpdate" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclPolicyUpdate(Bootstrap acls): failed " ++ e)
      Right aclBootstrapResponse -> do
        -- extract token from response
        let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
        -- create another consul client that is authenticated using the new token
        secondClient@ConsulClient{..} <-
          newClientAuth
            (consulServerHandleHttpPort consulServerHandle)
            token
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
        policy <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case policy of
          Left e -> expectationFailure ("AclPolicyUpdate: failed " ++ e)
          Right aclPolicyResponse -> do
            let updatedPolicyRequest =
                  AclPolicy
                    { aclPolicyId = (aclPolicyId aclPolicyResponse)
                    , aclPolicyName = updatedPolicyName 
                    , aclPolicyDescription = (aclPolicyDescription aclPolicyResponse)
                    , aclPolicyRules = (aclPolicyRules aclPolicyResponse)
                    , aclPolicyDatacenters = (aclPolicyDatacenters aclPolicyResponse)
                    , aclPolicyHash = (aclPolicyHash aclPolicyResponse)
                    , aclPolicyCreateIndex = (aclPolicyCreateIndex aclPolicyResponse)
                    , aclPolicyModifyIndex = (aclPolicyModifyIndex aclPolicyResponse)
                    }
            updatedPolicy <- aclPolicyUpdate secondClient { ccDatacenter = dc1 } updatedPolicyRequest
            case updatedPolicy of
              Left e -> expectationFailure ("AclPolicyUpdate: updated policy failed " ++ e)
              Right updatedPolicyResponse -> do
                context "AclPolicyUpdate: successful" $ (aclPolicyName updatedPolicyResponse) `shouldBe` updatedPolicyName


