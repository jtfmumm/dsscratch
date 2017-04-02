# Distributed-Systems-Scratchpad

A system for experimenting with implementing distributed systems.

So far it includes:  
* Processes, Channels, and Topologies  
* A TopologyRunner that simulates time and concurrency
* Nodes that can be augmented with components for encapsulating state and behavior  
* A NodeModule template for quickly implementing new components
  * NodeModules allow algorithm decoupling. For example, the Two Phase Commit component sends a Broadcast command that will be executed by whatever
  broadcast component is present.
* Lamport clocks and vector clocks  
* The Tarry algorithm for finding a spanning tree of a network  
* The Chandy-Lamport snapshot algorithm  
* Simple broadcast (flooding) and echo algorithms
* Two Phase Commit
* A test module to check whether broadcast algorithms work
* A test module to check whether commit/consensus algorithms work

### Overview

The Distributed Systems Scratchpad is based on the following model:

Multiple `Nodes` communicate over lossless FIFO `Channels`. Each `Node` encapsulates one or more `NodeModules` that represent the logic executed in response to messages (and potentially timers) and encapsulates local state used in that logic. A `Message` received at a `Node` over a `Channel` is delivered to each of that `Node's` `NodeModules`. A `NodeModule` will either execute code after receiving the `Message` (and potentially send an outgoing `Message` to its encapsulating `Node`) or ignore the `Message` altogether.

Each `Message` contains a `Command` that a `NodeModule` will use to determine what to do next. For example, `Echo.scala` defines a simple
echo algorithm for broadcasting over the cluster. In its `processMessage()`
method, it looks for one of three `Commands`: `Broadcast`, `InitiateEcho`,
or `Echo`. Any other `Commands` it ignores.

`NodeModuleTemplate.scala` provides some guidelines for creating a 
`NodeModule`.

