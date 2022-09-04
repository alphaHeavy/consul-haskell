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


  it "AclPolicyReadById" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclPolicyReadById(Bootstrap acls): failed " ++ e)
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
        aclPolicyResponse <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case aclPolicyResponse of
          Left e -> expectationFailure ("AclPolicyReadById: failed " ++ e)
          Right policy -> do
            readItBack <- aclPolicyReadById (secondClient { ccDatacenter = dc1 }) (aclPolicyId policy)
            case readItBack of
              Left e -> expectationFailure ("AclPolicyReadById: failed " ++ e)
              Right p -> do
                context "AclPolicyReadById: successful" $ (aclPolicyName p) `shouldBe` policyName


  it "AclPolicyReadByName" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclPolicyReadById(Bootstrap acls): failed " ++ e)
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
        aclPolicyResponse <- aclPolicyCreate secondClient { ccDatacenter = dc1 } policyRequest
        case aclPolicyResponse of
          Left e -> expectationFailure ("AclPolicyReadById: failed " ++ e)
          Right policy -> do
            readItBack <- aclPolicyReadByName (secondClient { ccDatacenter = dc1 }) (aclPolicyName policy)
            case readItBack of
              Left e -> expectationFailure ("AclPolicyReadById: failed " ++ e)
              Right p -> do
                context "AclPolicyReadById: successful" $ (aclPolicyName p) `shouldBe` policyName


  it "AclPolicyList" $ \consulServerHandle -> do
    client@ConsulClient{..} <- newClient $ consulServerHandleHttpPort consulServerHandle
    response <- aclBootstrap client{ ccDatacenter = dc1  }
    case response of
      Left e -> expectationFailure ("AclPolicyList(Bootstrap acls): failed " ++ e)
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
        p1 <- aclPolicyCreate (secondClient { ccDatacenter = dc1 }) $ policyRequests !! 0
        p2 <- aclPolicyCreate (secondClient { ccDatacenter = dc1 }) $ policyRequests !! 1
        p3 <- aclPolicyCreate (secondClient { ccDatacenter = dc1 }) $ policyRequests !! 2
        policies <- aclPolicyList secondClient { ccDatacenter = dc1 }
        print policies
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


