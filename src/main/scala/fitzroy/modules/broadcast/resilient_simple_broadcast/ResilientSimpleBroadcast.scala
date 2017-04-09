package fitzroy.modules.broadcast.resilient_simple_broadcast

import fitzroy.modules._
import fitzroy.modules.broadcast._
import fitzroy.clocks.TimeStamp
import fitzroy.components._
import fitzroy.timers._
import fitzroy.util.mLRUSet

import scala.collection.mutable.{Set => mSet, Map => mMap}

/*
If a node wants to broadcast a message, it sends it over every outgoing
channel wrapped in a Broadcast command. It periodically resends over each
channel until it receives an ack for that channel.

Each other node sends the received broadcast command over every outgoing
channel in the same way. Upon receiving it, the node delivers the contained
message to itself.

There is no guarantee messages will be received in a consistent order.
*/

trait ResilientSimpleBroadcastLocalState extends LocalState {
  var processedCommands: mMap[Command, Timer]
  var waitingForAck: mMap[Command, mSet[ProcessId]]
}

object ResilientSimpleBroadcastModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): ResilientSimpleBroadcastModule = {
    new ResilientSimpleBroadcastModule(parent)
  }
  def buildWith(parent: ModuleParent, s: ResilientSimpleBroadcastLocalState): ResilientSimpleBroadcastModule = {
    val newC = new ResilientSimpleBroadcastModule(parent)

    newC.s.processedCommands = s.processedCommands
    newC.s.waitingForAck = s.waitingForAck
    newC
  }
}

class ResilientSimpleBroadcastModule(val parent: ModuleParent) extends Module {
  val moduleCode: ModuleCode = ModuleCodes.RESILIENT_SIMPLE_BROADCAST

  ////////////////////
  //LOCAL STATE
  private object s extends ResilientSimpleBroadcastLocalState {
    var processedCommands = mMap[Command, Timer]()
    var waitingForAck = mMap[Command, mSet[ProcessId]]()
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit = {
    cmd match {
      case Broadcast(cmd, _, _) => {
        broadcastCommand(cmd)
      }
      case SimpleBroadcast(cmd, senderId, _) => {
        if (!s.processedCommands.contains(cmd) && (senderId != parent.id))
        {
          parent.deliver(cmd)
          broadcastCommand(cmd)
        }
        // Ack receipt
        parent.send(senderId, AckBroadcast(cmd, parent.id, clock.stamp()))
      }
      case AckBroadcast(cmd, ackerId, ts) => {
        if (s.waitingForAck(cmd).contains(ackerId)) {
          s.waitingForAck(cmd) -= ackerId
          if (s.waitingForAck(cmd).isEmpty) {
            val timer = s.processedCommands(cmd)
            parent.cancelTimer(timer)
          }
        }
      }
      case _ => // Ignore the rest
    }
  }

  def terminated: Boolean = {
    if (s.processedCommands.isEmpty) return false

    (for ((cmd, _) <- s.processedCommands) yield { terminatedFor(cmd) })
      .forall(b => b)
  }

  def terminatedFor(cmd: Command): Boolean =
    s.processedCommands.contains(cmd) && s.waitingForAck(cmd).isEmpty

  def step(): Unit = {
    //nothing to do...
  }

  def broadcastCommand(cmd: Command): Unit = {
    if (!s.processedCommands.contains(cmd)) {
      val timer = parent.setTimer(3, () => broadcastCommand(cmd), true)
      s.processedCommands(cmd) = timer
      s.waitingForAck(cmd) = mSet(outProcessIds:_*)
    }
    val newBroadcast = SimpleBroadcast(cmd, parent.id, clock.stamp())
    for (id <- outProcessIds)
      if (s.waitingForAck(cmd).contains(id))
        parent.send(id, newBroadcast)
  }

  def snapshot: ResilientSimpleBroadcastModule =
    ResilientSimpleBroadcastModule.buildWith(parent, s)

  def result = ""
}
