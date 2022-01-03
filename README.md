[![Build Status](https://travis-ci.org/alphaHeavy/consul-haskell.svg?branch=master)](https://travis-ci.org/alphaHeavy/consul-haskell)

# `consul-haskell` [package](https://hackage.haskell.org/package/consul-haskell): A consul client for Haskell

This package is a work-in-progress, but much of the functionality works.

## API Status Matrix

Here's a matrix with the status of support for each of Consul's APIs.

### Matrix Key

* ✔️  - Supported/Implemented.
* 🚧 — Under development (WIP).
* ✖️ - Not yet supported (but planned).
* ?: Don't know when we'll get to it, if ever.


| Section | API Name | Status | Test Status |
|---------|----------|--------|-------------|
| [ACLs](https://www.consul.io/api-docs/acl) |  | ✖️  | ✖️  |
|  | [Bootstrap ACLs](https://www.consul.io/api-docs/acl#bootstrap-acls) | ✖️  | ✖️  |
|  | [Check ACL Replication](https://www.consul.io/api-docs/acl#check-acl-replication) | ✖️  | ✖️  |
|  | [Translate Rules](https://www.consul.io/api-docs/acl#translate-rules) | ✖️  | ✖️  |
|  | [Translate a Legacy Token's Rules](https://www.consul.io/api-docs/acl#translate-a-legacy-token-s-rules) | ✖️  | ✖️  |
|  | [Login to Auth Method](https://www.consul.io/api-docs/acl#login-to-auth-method) | ✖️  | ✖️  |
|  | [Logout from Auth Method](https://www.consul.io/api-docs/acl#logout-from-auth-method) | ✖️  | ✖️  |
|  | [OIDC Authorization URL Request](https://www.consul.io/api-docs/acl#oidc-authorization-url-request) | ? | ? |
|  | [OIDC Callback](https://www.consul.io/api-docs/acl#oidc-callback) | ? | ? |
| [Admin Partitions](https://www.consul.io/api-docs/admin-partitions) |  | ? | ? |
|  | [Create a Partition](https://www.consul.io/api-docs/admin-partitions#create-a-partition) | ? | ? |
|  | [Read a Partition](https://www.consul.io/api-docs/admin-partitions#read-a-partition) | ? | ? |
|  | [Update a Partition](https://www.consul.io/api-docs/admin-partitions#update-a-partition) | ? | ? |
|  | [Delete a Partition](https://www.consul.io/api-docs/admin-partitions#delete-a-partition) | ? | ? |
|  | [List all Partitions](https://www.consul.io/api-docs/admin-partitions#list-all-partitions) | ? | ? |
| [Agent](https://www.consul.io/api-docs/agent) |  | 🚧 | 🚧 |
|  | [Retrieve host information](https://www.consul.io/api-docs/agent#retrieve-host-information) | ✔️  | ✔️  |
|  | [List Members](https://www.consul.io/api-docs/agent#list-members) | ✖️  | ✖️  |
|  | [Read Configuration](https://www.consul.io/api-docs/agent#read-configuration) | ✖️  | ✖️  |
|  | [Reload Agent](https://www.consul.io/api-docs/agent#reload-agent) | ✖️  | ✖️  |
|  | [Enable Maintenance Mode](https://www.consul.io/api-docs/agent#enable-maintenance-mode) | ✖️  | ✖️  |
|  | [View Metrics](https://www.consul.io/api-docs/agent#view-metrics) | ✖️  | ✖️  |
|  | [Stream Logs](https://www.consul.io/api-docs/agent#stream-logs) | ✖️  | ✖️  |
|  | [Join Agent](https://www.consul.io/api-docs/agent#join-agent) | ✖️  | ✖️  |
|  | [Graceful Leave and Shutdown](https://www.consul.io/api-docs/agent#graceful-leave-and-shutdown) | ✖️  | ✖️  |
|  | [Force Leave and Shutdown](https://www.consul.io/api-docs/agent#force-leave-and-shutdown) | ✖️  | ✖️  |
|  | [Update ACL Tokens](https://www.consul.io/api-docs/agent#update-acl-tokens) | ✖️  | ✖️  |
| [Agent Check](https://www.consul.io/api-docs/agent/check) |  | ✖️  | ✖️  |
|  | [List Checks](https://www.consul.io/api-docs/agent/check#list-checks) | ✖️  | ✖️  |
|  | [Register Check](https://www.consul.io/api-docs/agent/check#register-check) | ✖️  | ✖️  |
|  | [Deregister Check](https://www.consul.io/api-docs/agent/check#deregister-check) | ✖️  | ✖️  |
|  | [TTL Check Pass](https://www.consul.io/api-docs/agent/check#ttl-check-pass) | ✖️  | ✖️  |
|  | [TTL Check Warn](https://www.consul.io/api-docs/agent/check#ttl-check-warn) | ✖️  | ✖️  |
|  | [TTL Check Fail](https://www.consul.io/api-docs/agent/check#ttl-check-fail) | ✖️  | ✖️  |
|  | [TTL Check Update](https://www.consul.io/api-docs/agent/check#ttl-check-update) | ✖️  | ✖️  |
| [Agent Service](https://www.consul.io/api-docs/agent/service) |  | 🚧 | 🚧 |
|  | [List Services](https://www.consul.io/api-docs/agent/service#list-services) | ✔️  | ✔️  |
|  | [Get Service Configuration](https://www.consul.io/api-docs/agent/service#get-service-configuration) | ✔️  | ✔️  |
|  | [Get local service health](https://www.consul.io/api-docs/agent/service#get-local-service-health) | ✔️  | ✔️  |
|  | [Get local service health by ID](https://www.consul.io/api-docs/agent/service#get-local-service-health-by-its-id) | ✖️  | ✖️  |
|  | [Register Service](https://www.consul.io/api-docs/agent/service#register-service) | ✔️  | ✔️  |
|  | [Deregister Service](https://www.consul.io/api-docs/agent/service#deregister-service) | ✔️  | ✔️  |
|  | [Enable Maintenance Mode](https://www.consul.io/api-docs/agent/service#enable-maintenance-mode) | ✖️  | ✖️  |
| [Agent Connect](https://www.consul.io/api-docs/agent/connect) |  | ✖️  | ✖️  |
|  | [Authorize](https://www.consul.io/api-docs/agent/connect#authorize) | ✖️  | ✖️  |
|  | [Certificate Authority (CA) Roots](https://www.consul.io/api-docs/agent/connect#certificate-authority-ca-roots) | ✖️  | ✖️  |
|  | [Service Leaf Certificate](https://www.consul.io/api-docs/agent/connect#service-leaf-certificate) | ✖️  | ✖️  |
| [Catalog](https://www.consul.io/api-docs/catalog) |  | 🚧 | 🚧 |
|  | [Register Entity](https://www.consul.io/api-docs/catalog#register-entity) | ✖️  | ✖️  |
|  | [Deregister Entity](https://www.consul.io/api-docs/catalog#deregister-entity) | ✖️  | ✖️  |
|  | [List Datacenters](https://www.consul.io/api-docs/catalog#list-datacenters) | ✔️  | ✔️  |
|  | [List Nodes](https://www.consul.io/api-docs/catalog#list-nodes) | ✖️  | ✖️  |
|  | [List Services](https://www.consul.io/api-docs/catalog#list-services) | ✖️  | ✖️  |
|  | [List Nodes for Service](https://www.consul.io/api-docs/catalog#list-nodes-for-service) | ✖️  | ✖️  |
|  | [List Nodes for Connect-capable Service](https://www.consul.io/api-docs/catalog#list-nodes-for-connect-capable-service) | ✖️  | ✖️  |
|  | [Retrieve Map of Services for a Node](https://www.consul.io/api-docs/catalog#retrieve-map-of-services-for-a-node) | ✖️  | ✖️  |
|  | [List Services for Node](https://www.consul.io/api-docs/catalog#list-services-for-node) | ✖️  | ✖️  |
|  | [List Services for Gateway](https://www.consul.io/api-docs/catalog#list-services-for-gateway) | ✖️  | ✖️  |
| [Config](https://www.consul.io/api-docs/config) |  | ✖️  | ✖️  |
|  | [Apply Configuration](https://www.consul.io/api-docs/config#apply-configuration) | ✖️  | ✖️  |
|  | [Get Configuration](https://www.consul.io/api-docs/config#get-configuration) | ✖️  | ✖️  |
|  | [List Configurations](https://www.consul.io/api-docs/config#list-configurations) | ✖️  | ✖️  |
|  | [Delete Configuration](https://www.consul.io/api-docs/config#delete-configuration) | ✖️  | ✖️  |
| [Connect](https://www.consul.io/api-docs/connect) |  | ✖️  | ✖️  |
| [Connect Certificate Authority (CA)](https://www.consul.io/api-docs/connect/ca) |  | ✖️  | ✖️  |
|  | [List CA Root Certificates](https://www.consul.io/api-docs/connect/ca#list-ca-root-certificates) | ✖️  | ✖️  |
|  | [Get CA Configuration](https://www.consul.io/api-docs/connect/ca#get-ca-configuration) | ✖️  | ✖️  |
|  | [Update CA Configuration](https://www.consul.io/api-docs/connect/ca#update-ca-configuration) | ✖️  | ✖️  |
| [Connect Intentions](https://www.consul.io/api-docs/connect/intentions) |  | ✖️  | ✖️  |
|  | [Upsert Intention by Name](https://www.consul.io/api-docs/connect/intentions#upsert-intention-by-name) | ✖️  | ✖️  |
|  | [Create Intention with ID](https://www.consul.io/api-docs/connect/intentions#create-intention-with-id) | ✖️  | ✖️  |
|  | [Update Intention by ID](https://www.consul.io/api-docs/connect/intentions#update-intention-by-id) | ✖️  | ✖️  |
|  | [Read Specific Intention by Name](https://www.consul.io/api-docs/connect/intentions##read-specific-intention-by-name) | ✖️  | ✖️  |
|  | [Read Specific Intention by ID](https://www.consul.io/api-docs/connect/intentions#read-specific-intention-by-id) | ✖️  | ✖️  |
|  | [List Intentions](https://www.consul.io/api-docs/connect/intentions#list-intentions) | ✖️  | ✖️  |
|  | [Delete Intention by Name](https://www.consul.io/api-docs/connect/intentions#delete-intention-by-name) | ✖️  | ✖️  |
|  | [Delete Intention by ID](https://www.consul.io/api-docs/connect/intentions#delete-intention-by-id) | ✖️  | ✖️  |
|  | [Check Intention Result](https://www.consul.io/api-docs/connect/intentions#check-intention-result) | ✖️  | ✖️  |
|  | [List Matching Intentions](https://www.consul.io/api-docs/connect/intentions#list-matching-intentions) | ✖️  | ✖️  |
| [Coordinates](https://www.consul.io/api-docs/coordinate) |  | 🚧 | 🚧 |
|  | [Read WAN Coordinates](https://www.consul.io/api-docs/coordinate#read-wan-coordinates) | 🚧 | 🚧 |
|  | [Read LAN Coordinates for all nodes](https://www.consul.io/api-docs/coordinate#read-lan-coordinates-for-all-nodes) | 🚧 | 🚧 |
|  | [Read LAN Coordinates for a node](https://www.consul.io/api-docs/coordinate#read-lan-coordinates-for-a-node) | 🚧 | 🚧 |
|  | [Update LAN Coordinates for a node](https://www.consul.io/api-docs/coordinate#update-lan-coordinates-for-a-node) | 🚧 | 🚧 |
| [Discovery Chain](https://www.consul.io/api-docs/discovery-chain) |  | ✖️  | ✖️  |
|  | [Read Compiled Discovery Chain](https://www.consul.io/api-docs/discovery-chain#read-compiled-discovery-chain) | ✖️  | ✖️  |
| [Events](https://www.consul.io/api-docs/event) |  | ✖️  | ✖️  |
|  | [Fire Event](https://www.consul.io/api-docs/event#fire-event) | ✖️  | ✖️  |
|  | [List Events](https://www.consul.io/api-docs/event#list-events) | ✖️  | ✖️  |
| [Health](https://www.consul.io/api-docs/health) |  | 🚧 | 🚧 |
|  | [List Checks for Node](https://www.consul.io/api-docs/health#list-checks-for-node) | ✖️  | ✖️  |
|  | [List Checks for Service](https://www.consul.io/api-docs/health#list-checks-for-service) | ✔️  | ✔️  |
|  | [List Service Instances for Service](https://www.consul.io/api-docs/health#list-nodes-for-service) | ✔️  | ✔️  |
|  | [List Service Instances for Connect-enabled Service](https://www.consul.io/api-docs/health#list-service-instances-for-connect-enabled-service) | ✖️  | ✖️  |
|  | [List Service Instances for Ingress Gateways Associated with a Service](https://www.consul.io/api-docs/health#list-service-instances-for-ingress-gateways-associated-with-a-service) | ✖️  | ✖️  |
|  | [List Checks in State](https://www.consul.io/api-docs/health#list-checks-in-state) | ✖️  | ✖️  |
| [KV Store](https://www.consul.io/api-docs/kv) |  | ✔️  | ✔️  |
|  | [Read Key](https://www.consul.io/api-docs/kv#read-key) | ✔️  | ✔️  |
|  | [Create/Update Key](https://www.consul.io/api-docs/kv#create-update-key) | ✔️  | ✔️  |
|  | [Delete Key](https://www.consul.io/api-docs/kv#delete-key) | ✔️  | ✔️  |
| [Operator](https://www.consul.io/api-docs/operator) |  | ✖️  | ✖️  |
| [Operator Network Areas](https://www.consul.io/api-docs/operator/area) |  | ? | ? |
|  | [Create Network Area](https://www.consul.io/api-docs/operator/area#create-network-area) | ? | ? |
|  | [List Network Areas](https://www.consul.io/api-docs/operator/area#list-network-areas) | ? | ? |
|  | [Update Network Area](https://www.consul.io/api-docs/operator/area#update-network-area) | ? | ? |
|  | [List Specific Network Area](https://www.consul.io/api-docs/operator/area#list-specific-network-area) | ? | ? |
|  | [Delete Network Area](https://www.consul.io/api-docs/operator/area#delete-network-area) | ? | ? |
|  | [Join Network Area](https://www.consul.io/api-docs/operator/area#join-network-area) | ? | ? |
|  | [List Network Area Members](https://www.consul.io/api-docs/operator/area#list-network-area-members) | ? | ? |
| [Operator Autopilot](https://www.consul.io/api-docs/operator/autopilot) |  | ? | ? |
|  | [Read Configuration](https://www.consul.io/api-docs/operator/autopilot#read-configuration) | ? | ? |
|  | [Update Configuration](https://www.consul.io/api-docs/operator/autopilot#update-configuration) | ? | ? |
|  | [Read Health](https://www.consul.io/api-docs/operator/autopilot#read-health) | ? | ? |
|  | [Read the Autopilot State](https://www.consul.io/api-docs/operator/autopilot#read-the-autopilot-state) | ? | ? |
| [Operator Keyring](https://www.consul.io/api-docs/operator/keyring) |  | ? | ? |
|  | [List Gossip Encryption Keys](https://www.consul.io/api-docs/operator/keyring#list-gossip-encryption-keys) | ? | ? |
|  | [Add New Gossip Encryption Key](https://www.consul.io/api-docs/operator/keyring#add-new-gossip-encryption-key) | ? | ? |
|  | [Change Primary Gossip Encryption Key](https://www.consul.io/api-docs/operator/keyring#change-primary-gossip-encryption-key) | ? | ? |
|  | [Delete Gossip Encryption Key](https://www.consul.io/api-docs/operator/keyring#delete-gossip-encryption-key) | ? | ? |
| [Operator License](https://www.consul.io/api-docs/operator/license) |  | ? | ? |
|  | [Getting the Consul License](https://www.consul.io/api-docs/operator/license#getting-the-consul-license) | ? | ? |
| [Operator Raft](https://www.consul.io/api-docs/operator/raft) |  | ✖️  | ✖️  |
|  | [Read Configuration](https://www.consul.io/api-docs/operator/raft#read-configuration) | ✖️  | ✖️  |
|  | [Delete Raft Peer](https://www.consul.io/api-docs/operator/raft#delete-raft-peer) | ✖️  | ✖️  |
| [Operator Segment](https://www.consul.io/api-docs/operator/segment) |  | ? | ? |
|  | [List Network Segments](https://www.consul.io/api-docs/operator/segment#list-network-segments) | ? | ? |
| [Namespaces](https://www.consul.io/api-docs/namespaces) |  | ✖️  | ✖️  |
|  | [Create a Namespace](https://www.consul.io/api-docs/namespaces#create-a-namespace) | ✖️  | ✖️ |
|  | [Read a Namespace](https://www.consul.io/api-docs/namespaces#read-a-namespace) | ✖️ | ✖️ |
|  | [Update a Namespace](https://www.consul.io/api-docs/namespaces#update-a-namespace) | ✖️  | ✖️  |
|  | [Delete a Namespace](https://www.consul.io/api-docs/namespaces#delete-a-namespace) | ✖️  | ✖️  |
|  | [List all Namespaces](https://www.consul.io/api-docs/namespaces#list-all-namespaces) | ✖️  | ✖️  |
| [Prepared Queries](https://www.consul.io/api-docs/query) |  | ? | ? |
|  | [Create Prepared Query](https://www.consul.io/api-docs/query#create-prepared-query) | ? | ? |
|  | [List Prepared Queries](https://www.consul.io/api-docs/query#list-prepared-queries) | ? | ? |
|  | [Read Prepared Query](https://www.consul.io/api-docs/query#read-prepared-query) | ? | ? |
|  | [Delete Prepared Query](https://www.consul.io/api-docs/query#delete-prepared-query) | ? | ? |
|  | [Execute Prepared Query](https://www.consul.io/api-docs/query#execute-prepared-query) | ? | ? |
|  | [Explain Prepared Query](https://www.consul.io/api-docs/query#explain-prepared-query) | ? | ? |
| [Sessions](https://www.consul.io/api-docs/session) |  | 🚧  | 🚧  |
|  | [Create Session](https://www.consul.io/api-docs/session#create-session) | ✔️  | ✔️  |
|  | [Delete Session](https://www.consul.io/api-docs/session#delete-session) | ✔️  | ✔️  |
|  | [Read Session](https://www.consul.io/api-docs/session#read-session) | ✔️  | ✔️  |
|  | [List Sessions for Node](https://www.consul.io/api-docs/session#list-sessions-for-node) | ✖️  | ✖️  |
|  | [List Sessions](https://www.consul.io/api-docs/session#list-sessions) | ✖️  | ✖️  |
|  | [Renew Session](https://www.consul.io/api-docs/session#renew-session) | ✔️ | ✔️ |
| [Snapshots](https://www.consul.io/api-docs/snapshot) |  | ✖️  | ✖️  |
|  | [Generate Snapshot](https://www.consul.io/api-docs/snapshot#generate-snapshot) | ✖️  | ✖️  |
|  | [Restore Snapshot](https://www.consul.io/api-docs/snapshot#restore-snapshot) | ✖️  | ✖️  |
| [Status](https://www.consul.io/api-docs/status) |  | ✖️  | ✖️  |
|  | [Get Raft Leader](https://www.consul.io/api-docs/status#get-raft-leader) | ✖️  | ✖️  |
|  | [List Raft Peers](https://www.consul.io/api-docs/status#list-raft-peers) | ✖️  | ✖️  |
| [Transactions](https://www.consul.io/api-docs/txn) |  | ✖️  | ✖️  |
|  | [Create Transaction](https://www.consul.io/api-docs/txn#create-transaction) | ✖️  | ✖️  |

### Matrix Key

* ✔️  - Supported/Implemented.
* 🚧 — Under development (WIP).
* ✖️ - Not yet supported (but planned).
* ?: Don't know when we'll get to it, if ever.

## Contributing

For contributions, please:

* Make a pull request
* Make sure the tests pass
* Add a bullet to the Changelog

## Run Tests

Use `stack test`; you will need to have `consul` available in your `PATH`.

If you don't want to care about such dependencies, install [Nix](https://nixos.org/nix/), set up [NUR](https://github.com/nix-community/NUR) (see [`shell.nix`](shell.nix)) and run:

```bash
stack --nix-shell-file shell.nix test
```

which will provide all needed programs for you.

## History

The library was originally made by [Alpha Heavy Industries](https://github.com/alphaHeavy/).
Maintainance has been helped by @nh2, with partial time sponsoring from [FP Complete](https://tech.fpcomplete.com/haskell).
Development continued by @ketzacoatl.
