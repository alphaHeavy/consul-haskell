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
