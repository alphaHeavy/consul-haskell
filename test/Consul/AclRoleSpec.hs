{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consul.AclRoleSpec where

import Import

testsuiteSettingsWithAclsEnabled = 
  TestsuiteSettings
    { displayConsulServerLogs = False -- True
    , enableAcls = True
    , verboseLogs = False -- True
    }

spec :: Spec
spec = setupAround (consulServerSetupFuncWith testsuiteSettingsWithAclsEnabled) $ do

  it "AclRoleCreate" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclRoleCreate(Bootstrap acls): failed " ++ e)
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
                , consulApiRequestAclPolicyCreateDescription = "A policy for testing a role"
                , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
                , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
              --, consulApiRequestAclPolicyCreateNamespace = Nothing -- TODO: enterprise-only?
                }
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
            role <- aclRoleCreate secondClient { ccDatacenter = dc1 } roleRequest
            case role of
              Left e -> expectationFailure ("AclRoleCreate: failed " ++ e)
              Right aclRoleResponse ->
                context "AclRoleCreate: successful" $ (aclRoleName aclRoleResponse) `shouldBe` roleName

  it "AclRoleDelete" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    aclBootstrapResponse <- aclBootstrap client{ ccDatacenter = dc1  }
    case aclBootstrapResponse of
      Left e -> expectationFailure ("AclRoleCreate(Bootstrap acls): failed " ++ e)
      Right response -> do
        -- extract token from response
        let token = consulApiResponseAclBootstrapSecretId response
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
                , consulApiRequestAclPolicyCreateDescription = "A policy for testing a role"
                , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
                , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
              --, consulApiRequestAclPolicyCreateNamespace = Nothing -- TODO: enterprise-only?
                }
        -- use the second (authenticated) client to create acl policy
        aclPolicyResponse <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case aclPolicyResponse of
          Left e -> expectationFailure ("AclPolicyDelete: failed " ++ e)
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
              Left e -> expectationFailure ("AclRoleDelete: failed " ++ e)
              Right role -> do
                deleteResult <- liftIO $ aclRoleDelete secondClient (aclRoleId role)
                case deleteResult of
                  Left e -> expectationFailure ("AclRoleDelete: failed! " ++ e)
                  Right result ->
                    case result of
                      True -> context "AclRoleDelete: successful!" $ pure ()
                      False -> expectationFailure ("AclRoleDelete: failed to delete role!")


  it "AclRoleList" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclRoleList(Bootstrap acls): failed " ++ e)
      Right aclBootstrapResponse -> do
        -- extract token from response
        let token = consulApiResponseAclBootstrapSecretId aclBootstrapResponse
        -- create another consul client that is authenticated using the new token
        secondClient@ConsulClient{..} <-
          newClientAuth
            (consulServerHandleHttpPort consulServerHandle)
            token
        -- setup a role record
        let policyName = "Test-Role"
        let policyRequests =
              [ ConsulApiRequestAclPolicyCreate
                  { consulApiRequestAclPolicyCreateName = policyName <> "-1"
                  , consulApiRequestAclPolicyCreateDescription = "A role for testing"
                  , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
                  , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
                  }
              , ConsulApiRequestAclPolicyCreate
                  { consulApiRequestAclPolicyCreateName = policyName <> "-2"
                  , consulApiRequestAclPolicyCreateDescription = "A second role for testing"
                  , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
                  , consulApiRequestAclPolicyCreateDatacenters = ["dc2"]
                  }
              , ConsulApiRequestAclPolicyCreate
                  { consulApiRequestAclPolicyCreateName = policyName <> "-3"
                  , consulApiRequestAclPolicyCreateDescription = "A third role for testing"
                  , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"write\"}"
                  , consulApiRequestAclPolicyCreateDatacenters = ["dc3"]
                  }
              ]
        -- use the second (authenticated) client to create acl role
        --policies <- map (\p -> aclRoleCreate (secondClient { ccDatacenter = dc1 }) p) roleRequests 
        p1Response <- aclPolicyCreate (secondClient { ccDatacenter = dc1 }) $ policyRequests !! 0
        p2Response <- aclPolicyCreate (secondClient { ccDatacenter = dc1 }) $ policyRequests !! 1
        p3Response <- aclPolicyCreate (secondClient { ccDatacenter = dc1 }) $ policyRequests !! 2
        case p1Response of
          Left e -> expectationFailure ("AclRoleList(Create policy 1 for role 1): failed " ++ e)
          Right p1 -> do
            case p2Response of
              Left e -> expectationFailure ("AclRoleList(Create policy 2 for role 2): failed " ++ e)
              Right p2 -> do
                case p3Response of
                  Left e -> expectationFailure ("AclRoleList(Create policy 3 for role 3): failed " ++ e)
                  Right p3 -> do
                    -- create a AclPolicyLink for the Role's "policies" field
                    let policyLinks =
                          [ AclPolicyLink
                              { aclPolicyLinkId = Just (aclPolicyId p1) 
                              , aclPolicyLinkName = Nothing
                              }
                          , AclPolicyLink
                              { aclPolicyLinkId = Just (aclPolicyId p2) 
                              , aclPolicyLinkName = Just (aclPolicyName p2)
                              }
                          , AclPolicyLink
                              { aclPolicyLinkId = Nothing
                              , aclPolicyLinkName = Just (aclPolicyName p3)
                              }
                          ]
                    -- setup a role record
                    let roleName = "Test-Role"
                    -- create roles, each with their own policy
                    let roleRequests = 
                          [ ConsulApiRequestAclRoleCreate
                              { consulApiRequestAclRoleCreateName = roleName <> "-1"
                              , consulApiRequestAclRoleCreateDescription = "A role for testing"
                              , consulApiRequestAclRoleCreatePolicies = Just [policyLinks !! 0]
                              , consulApiRequestAclRoleCreateServiceIdentities = []
                              , consulApiRequestAclRoleCreateNodeIdentities = []
                            --, consulApiRequestAclRoleCreateNamespace = Nothing -- TODO: enterprise-only?
                              }
                          , ConsulApiRequestAclRoleCreate
                              { consulApiRequestAclRoleCreateName = roleName <> "-2"
                              , consulApiRequestAclRoleCreateDescription = "A role for testing"
                              , consulApiRequestAclRoleCreatePolicies = Just [policyLinks !! 1]
                              , consulApiRequestAclRoleCreateServiceIdentities = []
                              , consulApiRequestAclRoleCreateNodeIdentities = []
                            --, consulApiRequestAclRoleCreateNamespace = Nothing -- TODO: enterprise-only?
                              }
                          , ConsulApiRequestAclRoleCreate
                              { consulApiRequestAclRoleCreateName = roleName <> "-3"
                              , consulApiRequestAclRoleCreateDescription = "A role for testing"
                              , consulApiRequestAclRoleCreatePolicies = Just [policyLinks !! 2]
                              , consulApiRequestAclRoleCreateServiceIdentities = []
                              , consulApiRequestAclRoleCreateNodeIdentities = []
                            --, consulApiRequestAclRoleCreateNamespace = Nothing -- TODO: enterprise-only?
                              }
                          ]
                    r1 <- aclRoleCreate (secondClient { ccDatacenter = dc1 }) $ roleRequests !! 0
                    r2 <- aclRoleCreate (secondClient { ccDatacenter = dc1 }) $ roleRequests !! 1
                    r3 <- aclRoleCreate (secondClient { ccDatacenter = dc1 }) $ roleRequests !! 2
                    policies <- aclRoleList secondClient { ccDatacenter = dc1 }
                    policies <- aclRoleList secondClient { ccDatacenter = dc1 }
                    --print policies
                    case policies of
                      Left e -> expectationFailure ("AclRoleList: failed " ++ e)
                      Right aclRoleList -> do
                        --print aclRoleList
                        context "AclRoleList: successful" $ pure ()


  it "AclRoleReadById" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclRoleReadById(Bootstrap acls): failed " ++ e)
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
                , consulApiRequestAclPolicyCreateDescription = "A policy for testing a role"
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
              Left e -> expectationFailure ("AclRoleReadById: failed " ++ e)
              Right role -> do
                readItBack <- aclRoleReadById (secondClient { ccDatacenter = dc1 }) (aclRoleId role)
                case readItBack of
                  Left e -> expectationFailure ("AclRoleReadById: failed " ++ e)
                  Right p -> do
                    context "AclRoleReadById: successful" $ (aclRoleName p) `shouldBe` roleName


  it "AclRoleReadByName" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclRoleReadById(Bootstrap acls): failed " ++ e)
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
                , consulApiRequestAclPolicyCreateDescription = "A policy for testing a role"
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
              Left e -> expectationFailure ("AclRoleReadById: failed " ++ e)
              Right role -> do
                readItBack <- aclRoleReadByName (secondClient { ccDatacenter = dc1 }) (aclRoleName role)
                case readItBack of
                  Left e -> expectationFailure ("AclRoleReadById: failed " ++ e)
                  Right p -> do
                    context "AclRoleReadById: successful" $ (aclRoleName p) `shouldBe` roleName


  it "AclRoleUpdate" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclRoleUpdate(Bootstrap acls): failed " ++ e)
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
                , consulApiRequestAclPolicyCreateDescription = "A policy for testing a role"
                , consulApiRequestAclPolicyCreateRules = "node_prefix \"\" { policy = \"read\"}"
                , consulApiRequestAclPolicyCreateDatacenters = ["dc1"] -- None is all
              --, consulApiRequestAclPolicyCreateNamespace = Nothing -- TODO: enterprise-only?
                }
        -- use the second (authenticated) client to create acl policy
        aclPolicyResponse <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case aclPolicyResponse of
          Left e -> expectationFailure ("AclPolicyUpdate: failed " ++ e)
          Right policy -> do
            -- create a AclPolicyLink for the Role's "policies" field
            let policyLink = AclPolicyLink
                  { aclPolicyLinkId = Just (aclPolicyId policy) 
                  , aclPolicyLinkName = Nothing
                  }
            -- setup a role record
            let roleName = "Test-Role"
            let updatedRoleName = "Updated-Role-Name"
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
            role <- aclRoleCreate secondClient { ccDatacenter = dc1 } roleRequest
            case role of
              Left e -> expectationFailure ("AclRoleUpdate: failed " ++ e)
              Right aclRoleResponse -> do
                let updatedRoleRequest =
                      AclRole
                        { aclRoleId = (aclRoleId aclRoleResponse)
                        , aclRoleName = updatedRoleName 
                        , aclRoleDescription = (aclRoleDescription aclRoleResponse)
                        , aclRolePolicies = Just [policyLink]
                        , aclRoleServiceIdentities = Nothing
                        , aclRoleNodeIdentities = Nothing
                        , aclRoleHash = (aclRoleHash aclRoleResponse)
                        , aclRoleCreateIndex = (aclRoleCreateIndex aclRoleResponse)
                        , aclRoleModifyIndex = (aclRoleModifyIndex aclRoleResponse)
                        }
                updatedRoleResponse <- aclRoleUpdate secondClient { ccDatacenter = dc1 } updatedRoleRequest
                case updatedRoleResponse of
                  Left e -> expectationFailure ("AclRoleUpdate: updated role failed " ++ e)
                  Right updatedRole -> do
                    context "AclRoleUpdate: successful" $ (aclRoleName updatedRole) `shouldBe` updatedRoleName


