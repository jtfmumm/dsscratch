package dsscratch.algos.broadcast

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.clocks.TimeStamp
import dsscratch.components._
import dsscratch.util.mLRUSet

/*
If a node wants to broadcast a message, it sends it over every outgoing
channel exactly once wrapped in a Broadcast command.

Each other node sends the received broadcast command over every outgoing
channel exactly once. Upon receiving it, the node delivers the contained
message to itself.

If a node or channel fails, the message may not reach every node.
There is no guarantee messages will be received in a consistent order.
*/

trait SimpleBroadcastLocalState extends LocalState {
  var processedCommands: mLRUSet[Command]
}

object SimpleBroadcastModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): SimpleBroadcastModule = {
    new SimpleBroadcastModule(parentNode)
  }
  def buildWith(parentNode: Node, s: SimpleBroadcastLocalState): SimpleBroadcastModule = {
    val newC = new SimpleBroadcastModule(parentNode)

    newC.s.processedCommands = s.processedCommands
    newC
  }
}

class SimpleBroadcastModule(val parentNode: Node) extends NodeModule {
  val algoCode: AlgoCode = AlgoCodes.SIMPLE_BROADCAST

  ////////////////////
  //LOCAL STATE
  private object s extends SimpleBroadcastLocalState {
    var processedCommands = mLRUSet[Command]()
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit = {
    cmd match {
      case Broadcast(cmd, senderId, ts) => {
        val simpleBroadcast = SimpleBroadcast(cmd, senderId, ts)
        parentNode.deliver(Message(simpleBroadcast, parentNode.id,
          clock.stamp()))
      }
      case SimpleBroadcast(cmd, _, _) => {
        if (s.processedCommands.contains(cmd)) return
        val unwrapped = Message(cmd, parentNode.id, clock.stamp())
        if (chSenderId != parentNode.id) parentNode.deliver(unwrapped)
        s.processedCommands += cmd

        broadcastMessage(SimpleBroadcast(cmd, parentNode.id, clock.stamp()))
      }
      case _ => // Ignore the rest
    }
  }

  def terminated: Boolean = {
    if (s.processedCommands.isEmpty) return false

    (for (cmd <- s.processedCommands.lru) yield { terminatedFor(cmd) })
      .forall(b => b)
  }

  def terminatedFor(cmd: Command): Boolean = s.processedCommands.contains(cmd)

  def step(): Unit = {
    //nothing to do...
  }

  def broadcastMessage(cmd: Command): Unit = {
    val newBroadcast = Message(cmd, parentNode.id, clock.stamp())
    for (id <- outProcessIds) parentNode.send(id, newBroadcast)
  }

  def snapshot: SimpleBroadcastModule =
    SimpleBroadcastModule.buildWith(parentNode, s)

  def result = ""
}
