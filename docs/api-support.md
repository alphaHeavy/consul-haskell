#  API Support

Consul has many APIs, which are supported by `consul-haskell`?

* ACLs
* Agent
* Catalog
* Connect
* Coordinates
  * Read WAN Coordinates
  * Read LAN Coordinates for all Nodes
  * Read LAN Coordinates for a Node
  * Update LAN Coordinates for a Node
* Discovery Chain
  * Read Compiled Discovery Chain
* Events
  * Fire Event
  * List Events
* Health
  * List Checks for Node
  * List Checks for Service
  * List Nodes for Service
  * List Nodes for Connect-capable Service
  * List Nodes for Ingress Gateways Associated to a Service
  * List Checks in State
* KV Store
  * Read Key
  * Create / Update Key
  * Delete Key
* Operator
  * Area
  * Autopilot
  * Keyring
  * License
  * Raft
  * Segment
* Namespaces
  * Create a Namespace
  * Read a Namespace
  * Update a Namespace
  * Delete a Namepsace
  * List all Namespaces
* Prepared Queries
* Sessions
* Snapshots
* Status
* Transactions