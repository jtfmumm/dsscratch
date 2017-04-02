package dsscratch.algos.hearbeat

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.clocks._
import dsscratch.components._

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

object SimpleHeartbeatModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): SimpleHeartbeatModule = {
    new SimpleHeartbeatModule(parentNode)
  }
  def buildWith(parentNode: Node, s: SimpleHeartbeatLocalState):
    SimpleHeartbeatModule = {
    val newC = new SimpleHeartbeatModule(parentNode)
    newC.s.initiated = s.initiated
    newC
  }
}

class SimpleHeartbeatModule(val parentNode: Node) extends NodeModule {
  val algoCode: AlgoCode = AlgoCodes.SIMPLE_HEARTBEAT
  // A NodeModule can access the ids of Nodes it has channels to and
  // Nodes that have channels to its parent via the following methods
  // on the NodeModule trait:
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
    SimpleHeartbeatModule.buildWith(parentNode, s)

  def result = "" // For printing results

  private def sendHeartbeat(): Unit = {
    val ts = clock.stamp()
    val broadcast = Broadcast(Heartbeat(parentNode.id, ts), parentNode.id, ts)
    val msg = Message(broadcast, parentNode.id, ts)
    for (nodeId <- outProcessIds) parentNode.send(nodeId, msg)
  }

  private def initiate(): Unit = {
    val msg = Message(SendHeartbeat, parentNode.id, clock.stamp())
    parentNode.setTimer(50, () => parentNode.deliver(msg), true)
  }
}
