package fitzroy.modules.broadcast.simple_broadcast

import fitzroy.modules._
import fitzroy.modules.broadcast._
import fitzroy.clocks.TimeStamp
import fitzroy.components._
import fitzroy.util.mLRUSet

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

object SimpleBroadcastModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): SimpleBroadcastModule = {
    new SimpleBroadcastModule(parent)
  }
  def buildWith(parent: ModuleParent, s: SimpleBroadcastLocalState): SimpleBroadcastModule = {
    val newC = new SimpleBroadcastModule(parent)

    newC.s.processedCommands = s.processedCommands
    newC
  }
}

class SimpleBroadcastModule(val parent: ModuleParent) extends Module {
  val moduleCode: ModuleCode = ModuleCodes.SIMPLE_BROADCAST

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
        parent.deliver(simpleBroadcast)
      }
      case SimpleBroadcast(cmd, _, _) => {
        if (s.processedCommands.contains(cmd)) return
        if (chSenderId != parent.id) parent.deliver(cmd)
        s.processedCommands += cmd

        broadcastMessage(SimpleBroadcast(cmd, parent.id, clock.stamp()))
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
    for (id <- outProcessIds) parent.send(id, cmd)
  }

  def snapshot: SimpleBroadcastModule =
    SimpleBroadcastModule.buildWith(parent, s)

  def result = ""
}
