# Fitzroy

Distributed system simulator and distributed algorithm scratchpad.

* [Installation](#installation)
* [Fitzroy Model](#fitzroy-model)
* [Getting Started](#getting-started)
* [Illustration](#illustraction)

So far it includes:  
* Processes, Channels, and Topologies  
* A TopologyRunner that simulates time and concurrency
* Nodes that can be augmented with modules for encapsulating state and behavior 
* A Module template for quickly implementing new modules
  * Modules allow algorithm decoupling. For example, the Two Phase Commit
    module sends a Broadcast command that will be executed by whatever
    broadcast component is present.
* Lamport clocks and vector clocks  
* Simple broadcast (flooding) and echo algorithms
* Two Phase Commit
* A test module to check whether broadcast algorithms work
* A test module to check whether commit/consensus algorithms work
* The Tarry algorithm for finding a spanning tree of a network  
* The Chandy-Lamport snapshot algorithm  

## Installation

## Fitzroy Model

Multiple `Nodes` communicate over FIFO `Channels`. Each `Node` encapsulates one or more `Modules` that represent the logic executed in response to messages (and potentially timers) and encapsulate local state used in that logic. 

A `Message` received at a `Node` over a `Channel` is unwrapped and the `Command` it contains is delivered to each of that `Node's` `Modules`. A `Module` will either execute code after receiving the `Command` (and potentially send an outgoing `Command` via its encapsulating `Node`) or ignore the `Command` altogether.

For example, `Echo.scala` defines a simple echo algorithm for broadcasting over the cluster. In its `processMessage()` method, it looks for one of three `Commands`: `Broadcast`, `InitiateEcho`, or `Echo`. Any other `Commands` it ignores.

## Getting Started

In order to use Fitzroy to implement distributed algorithms, you will implement
a `Module`. You can look at `Module.scala` for the complete interface (and the
`ModuleParent` interface, which is how a `Module` communicates with its encapsulating `Node`). `ModuleTemplate.scala` provides a template and some guidelines for creating a `Module`. For a simple example, see 
`SimpleBroadcast.scala`.

In order to create a test of your algorithm, you will need to define a separate
test `Module`. For an example of such a test `Module`, see 
`BroadcastTester.scala`, which can be used to test broadcast algorithms.

You run tests via `ModuleTest`, through which you can define topology parameters as well as test parameters. See `SimpleBroadcastTesterRunner.scala`
for an example.

## Illustration

Fitzroy exploring the treacherous waters of distributed systems theory:
![Fitzroy](/images/fitzroy.gif?raw=true "Fitzroy")
