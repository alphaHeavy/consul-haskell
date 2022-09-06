{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.AclTokenSpec where

import Import

testsuiteSettingsWithAclsEnabled = 
  TestsuiteSettings
    { displayConsulServerLogs = False -- True
    , enableAcls = True
    , verboseLogs = False -- True
    }

spec :: Spec
spec = setupAround (consulServerSetupFuncWith testsuiteSettingsWithAclsEnabled) $ do

  it "AclTokenCreate" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclTokenCreate(Bootstrap acls): failed " ++ e)
      Right aclBootstrapResponse -> do
        let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
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
                }
        -- use the second (authenticated) client to create acl policy
        --policy <- withEntity secondClient aclPolicyCreate policyRequest createFailed
        --context "AclPolicyCreate: successful" $ (aclPolicyName policy) `shouldBe` policyName
        -- use the second (authenticated) client to create acl policy
        aclPolicyResponse <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case aclPolicyResponse of
          Left e -> expectationFailure ("AclPolicyCreate: failed " ++ e)
          Right policy -> do
            -- create a AclPolicyLink for the Role's "policies" field
            let policyLink = AclPolicyLink
                  { aclPolicyLinkId = Just (aclPolicyId policy) 
                  , aclPolicyLinkName = Nothing
                  }
            -- setup a role record
            let roleName = "Test-Role"
            let roleRequest =
                  ConsulApiRequestAclRoleCreate
                    { consulApiRequestAclRoleCreateName = roleName
                    , consulApiRequestAclRoleCreateDescription = "A role for testing"
                    , consulApiRequestAclRoleCreatePolicies = Just [policyLink]
                    , consulApiRequestAclRoleCreateServiceIdentities = []
                    , consulApiRequestAclRoleCreateNodeIdentities = []
                  --, consulApiRequestAclRoleCreateNamespace = Nothing -- TODO: enterprise-only?
                    }
            -- use the second (authenticated) client to create acl role
            aclRoleResponse <- aclRoleCreate secondClient { ccDatacenter = dc1 } roleRequest
            case aclRoleResponse of
              Left e -> expectationFailure ("AclRoleCreate: failed " ++ e)
              Right role -> do
                let roleLink = AclRoleLink
                      { aclRoleLinkId = Just (aclRoleId role) 
                      , aclRoleLinkName = Nothing
                      }
                let tokenDescription = "A role for testing"
                let tokenRequest =
                      ConsulApiRequestAclTokenCreate
                        { consulApiRequestAclTokenCreateAccessorId = Nothing
                        , consulApiRequestAclTokenCreateSecretId = Nothing
                        , consulApiRequestAclTokenCreateDescription = tokenDescription
                        , consulApiRequestAclTokenCreatePolicies = Just [policyLink]
                        , consulApiRequestAclTokenCreateRoles = Just [roleLink]
                        , consulApiRequestAclTokenCreateServiceIdentities = []
                        , consulApiRequestAclTokenCreateNodeIdentities = []
                        , consulApiRequestAclTokenCreateLocal = False
                        , consulApiRequestAclTokenCreateExpirationTime = Nothing
                        , consulApiRequestAclTokenCreateExpirationTtl = Nothing
                      --, consulApiRequestAclTokenCreateNamespace = Nothing
                    }
                aclTokenResponse <- aclTokenCreate secondClient { ccDatacenter = dc1 } tokenRequest
                case aclTokenResponse of
                  Left e -> expectationFailure ("AclTokenCreate: failed " ++ e)
                  Right token ->
                    context "AclTokenCreate: successful" $ (aclRoleLinkId $ (aclTokenRoleLinks token) !! 0) `shouldBe` (aclRoleLinkId roleLink)
                  --context "AclTokenCreate: successful" $ (aclTokenDescription token) `shouldBe` tokenDescription


  it "AclTokenDelete" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclTokenDelete(aclBootstrap): failed " ++ e)
      Right aclBootstrapResponse -> do
        let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
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
                }
        -- use the second (authenticated) client to create acl policy
        --policy <- withEntity secondClient aclPolicyCreate policyRequest createFailed
        --context "AclPolicyCreate: successful" $ (aclPolicyName policy) `shouldBe` policyName
        -- use the second (authenticated) client to create acl policy
        aclPolicyResponse <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case aclPolicyResponse of
          Left e -> expectationFailure ("AclTokenDelete(createPolicy): failed " ++ e)
          Right policy -> do
            -- create a AclPolicyLink for the Role's "policies" field
            let policyLink = AclPolicyLink
                  { aclPolicyLinkId = Just (aclPolicyId policy) 
                  , aclPolicyLinkName = Nothing
                  }
            -- setup a role record
            let roleName = "Test-Role"
            let roleRequest =
                  ConsulApiRequestAclRoleCreate
                    { consulApiRequestAclRoleCreateName = roleName
                    , consulApiRequestAclRoleCreateDescription = "A role for testing"
                    , consulApiRequestAclRoleCreatePolicies = Just [policyLink]
                    , consulApiRequestAclRoleCreateServiceIdentities = []
                    , consulApiRequestAclRoleCreateNodeIdentities = []
                  --, consulApiRequestAclRoleCreateNamespace = Nothing -- TODO: enterprise-only?
                    }
            -- use the second (authenticated) client to create acl role
            aclRoleResponse <- aclRoleCreate secondClient { ccDatacenter = dc1 } roleRequest
            case aclRoleResponse of
              Left e -> expectationFailure ("AclRoleCreate: failed " ++ e)
              Right role -> do
                let roleLink = AclRoleLink
                      { aclRoleLinkId = Just (aclRoleId role) 
                      , aclRoleLinkName = Nothing
                      }
                let tokenDescription = "A role for testing"
                let tokenRequest =
                      ConsulApiRequestAclTokenCreate
                        { consulApiRequestAclTokenCreateAccessorId = Nothing
                        , consulApiRequestAclTokenCreateSecretId = Nothing
                        , consulApiRequestAclTokenCreateDescription = tokenDescription
                        , consulApiRequestAclTokenCreatePolicies = Just [policyLink]
                        , consulApiRequestAclTokenCreateRoles = Just [roleLink]
                        , consulApiRequestAclTokenCreateServiceIdentities = []
                        , consulApiRequestAclTokenCreateNodeIdentities = []
                        , consulApiRequestAclTokenCreateLocal = False
                        , consulApiRequestAclTokenCreateExpirationTime = Nothing
                        , consulApiRequestAclTokenCreateExpirationTtl = Nothing
                      --, consulApiRequestAclTokenCreateNamespace = Nothing
                    }
                aclTokenResponse <- aclTokenCreate secondClient { ccDatacenter = dc1 } tokenRequest
                case aclTokenResponse of
                  Left e -> expectationFailure ("AclTokenCreate: failed " ++ e)
                  Right token -> do
                    deleteResult <- liftIO $ aclTokenDelete secondClient (aclTokenAccessorId token)
                    case deleteResult of
                      Left e -> expectationFailure ("AclTokenDelete: failed! " ++ e)
                      Right result ->
                        case result of
                          True -> context "AclTokenDelete: successful!" $ pure ()
                          False -> expectationFailure ("AclTokenDelete: failed to delete token!")

  it "AclTokenReadById" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclTokenReadById(aclBootstrap): failed " ++ e)
      Right aclBootstrapResponse -> do
        -- extract token from response
        let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
        -- create another consul client that is authenticated using the new token
        secondClient@ConsulClient{..} <-
          newClientAuth
            (consulServerHandleHttpPort consulServerHandle)
            token
        -- setup a policy for the role
        let policyName = "Test-Role-Policy"
        let policyRequest =
              ConsulApiRequestAclPolicyCreate
                { consulApiRequestAclPolicyCreateName = policyName
                , consulApiRequestAclPolicyCreateDescription = "A policy for testing a token"
                , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
                , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
              --, consulApiRequestAclPolicyCreateNamespace = Nothing -- TODO: enterprise-only?
                }
        -- use the second (authenticated) client to create acl policy
        aclPolicyResponse <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case aclPolicyResponse of
          Left e -> expectationFailure ("AclPolicyReadById: failed " ++ e)
          Right policy -> do
            -- create a AclPolicyLink for the Role's "policies" field
            let policyLink = AclPolicyLink
                  { aclPolicyLinkId = Just (aclPolicyId policy) 
                  , aclPolicyLinkName = Nothing
                  }
            -- setup a role record
            let roleName = "Test-Role"
            let roleRequest =
                  ConsulApiRequestAclRoleCreate
                    { consulApiRequestAclRoleCreateName = roleName
                    , consulApiRequestAclRoleCreateDescription = "A role for testing"
                    , consulApiRequestAclRoleCreatePolicies = Just [policyLink]
                    , consulApiRequestAclRoleCreateServiceIdentities = []
                    , consulApiRequestAclRoleCreateNodeIdentities = []
                  --, consulApiRequestAclRoleCreateNamespace = Nothing -- TODO: enterprise-only?
                    }
            -- use the second (authenticated) client to create acl role
            aclRoleResponse <- aclRoleCreate secondClient { ccDatacenter = dc1 } roleRequest
            case aclRoleResponse of
              Left e -> expectationFailure ("AclTokenReadById: failed " ++ e)
              Right role -> do
                let roleLink = AclRoleLink
                      { aclRoleLinkId = Just (aclRoleId role) 
                      , aclRoleLinkName = Nothing
                      }
                let tokenDescription = "A role for testing"
                let tokenRequest =
                      ConsulApiRequestAclTokenCreate
                        { consulApiRequestAclTokenCreateAccessorId = Nothing
                        , consulApiRequestAclTokenCreateSecretId = Nothing
                        , consulApiRequestAclTokenCreateDescription = tokenDescription
                        , consulApiRequestAclTokenCreatePolicies = Just [policyLink]
                        , consulApiRequestAclTokenCreateRoles = Just [roleLink]
                        , consulApiRequestAclTokenCreateServiceIdentities = []
                        , consulApiRequestAclTokenCreateNodeIdentities = []
                        , consulApiRequestAclTokenCreateLocal = False
                        , consulApiRequestAclTokenCreateExpirationTime = Nothing
                        , consulApiRequestAclTokenCreateExpirationTtl = Nothing
                      --, consulApiRequestAclTokenCreateNamespace = Nothing
                    }
                aclTokenResponse <- aclTokenCreate secondClient { ccDatacenter = dc1 } tokenRequest
                case aclTokenResponse of
                  Left e -> expectationFailure ("AclTokenReadById: failed " ++ e)
                  Right token -> do
                    readItBack <- aclTokenReadById (secondClient { ccDatacenter = dc1 }) (aclTokenAccessorId token)
                    case readItBack of
                      Left e -> expectationFailure ("AclTokenReadById: failed " ++ e)
                      Right t -> do
                        context "AclTokenReadById: successful" $ (aclRoleLinkId $ (aclTokenRoleLinks t) !! 0) `shouldBe` (aclRoleLinkId roleLink)

