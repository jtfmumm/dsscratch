package fitzroy.modules.heartbeat

import fitzroy.modules._
import fitzroy.modules.broadcast._
import fitzroy.clocks._
import fitzroy.components._

import scala.collection.mutable.ArrayBuffer

/*
Use this empty example as a template for creating new components.
Modules encapsulate algorithms and the related local state.
Find and replace SimpleHeartbeat with your component name in your copy of this file.

The parent Node of the component handles clock updates based on messages
received, so don't do that here.
*/

trait SimpleHeartbeatLocalState extends LocalState {
  var initiated: Boolean
}

object SimpleHeartbeatModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): SimpleHeartbeatModule = {
    new SimpleHeartbeatModule(parent)
  }
  def buildWith(parent: ModuleParent, s: SimpleHeartbeatLocalState):
    SimpleHeartbeatModule = {
    val newC = new SimpleHeartbeatModule(parent)
    newC.s.initiated = s.initiated
    newC
  }
}

class SimpleHeartbeatModule(val parent: ModuleParent) extends Module {
  val moduleCode: ModuleCode = ModuleCodes.SIMPLE_HEARTBEAT
  // A Module can access the ids of Nodes it has channels to and
  // Nodes that have channels to its parent via the following methods
  // on the Module trait:
  // -- outProcessIds
  // -- inProcessIds

  ////////////////////
  //LOCAL STATE
  private object s extends SimpleHeartbeatLocalState {
    var initiated: Boolean = false
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp):
    Unit =
  {
    cmd match {
      case SendHeartbeat => sendHeartbeat()
      case _ => // Do nothing
    }
  }

  def terminated: Boolean = false //never terminates

  def step(): Unit = {
    if (!s.initiated) initiate()
  }

  def snapshot: SimpleHeartbeatModule =
    SimpleHeartbeatModule.buildWith(parent, s)

  def result = "" // For printing results

  private def sendHeartbeat(): Unit = {
    val ts = clock.stamp()
    val broadcast = Broadcast(Heartbeat(parent.id, ts), parent.id, ts)
    for (nodeId <- outProcessIds) parent.send(nodeId, broadcast)
  }

  private def initiate(): Unit = {
    parent.setTimer(50, () => sendHeartbeat(), true)
  }
}
