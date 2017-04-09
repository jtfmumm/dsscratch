package fitzroy.modules.crdt.p_counter

import fitzroy.modules._
import fitzroy.modules.broadcast._
import fitzroy.modules.crdt._
import fitzroy.modules.gossip._
import fitzroy.components._
import fitzroy.clocks._
import fitzroy.client_server._
import fitzroy.datast.crdt._

import scala.collection.mutable.ArrayBuffer

trait PCounterLocalState extends LocalState {
  var pCounter: PCounter
  var clientConnections: Set[ClientConnection] = Set[ClientConnection]()
}

object PCounterModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): PCounterModule =
  {
    new PCounterModule(parent, nodeIds)
  }

  def buildWith(parent: ModuleParent, nodeIds: Set[ProcessId],
    s: PCounterLocalState): PCounterModule =
  {
    val newC = PCounterModule(parent, nodeIds)
    newC.s.pCounter = s.pCounter
    newC
  }
}

class PCounterModule(val parent: ModuleParent, nodeIds: Set[ProcessId])
  extends Module
{
  val moduleCode: ModuleCode = ModuleCodes.P_COUNTER

  ////////////////////
  //LOCAL STATE
  private object s extends PCounterLocalState {
    var pCounter: PCounter = PCounter(parent.id, nodeIds)
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit = {
    cmd match {
      case Request(cmd, conn) =>
        cmd match {
          case IncrementPCounter =>
            updateCounter(s.pCounter.inc())
            conn.response(updatedPCounterResponse)
          case MergePCounter(otherCounter) =>
            updateCounter(s.pCounter.merge(otherCounter))
            s.clientConnections.foreach(_.response(updatedPCounterResponse))
        }
      case StartGossip => startGossip()
      case _ => // Ignore the rest
    }
  }

  def terminated: Boolean = false //Never terminates

  def step(): Unit = {
    // Nothing to do
  }

  def snapshot: PCounterModule =
    PCounterModule.buildWith(parent, nodeIds, s)

  def result = "" // For printing results

  override def connectClient(conn: ClientConnection): Unit = {
    s.clientConnections += conn
  }

  override def disconnectClient(conn: ClientConnection): Unit = {
    s.clientConnections -= conn
  }

  private def updateCounter(p: PCounter): Unit = {
    s.pCounter = p
  }

  private def updatedPCounterResponse: Response = {
    Response(UpdatedPCounter(s.pCounter), parent.id, clock.stamp())
  }

  private def startGossip(): Unit = {
    val mergeCmd = MergePCounter(s.pCounter)
    val initiateBroadcast = Broadcast(mergeCmd, parent.id, clock.stamp())
    parent.deliver(initiateBroadcast)
  }
}
