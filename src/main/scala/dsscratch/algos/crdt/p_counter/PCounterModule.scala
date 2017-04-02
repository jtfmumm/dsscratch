package dsscratch.algos.crdt.p_counter

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.components._
import dsscratch.clocks._
import dsscratch.client_server._

import scala.collection.mutable.ArrayBuffer

trait PCounterLocalState extends LocalState {
  var pCounter: PCounter
  var clientConnections: Set[ClientConnection] = Set[ClientConnection]()
}

object PCounterModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): PCounterModule =
  {
    new PCounterModule(parentNode, nodeIds)
  }

  def buildWith(parentNode: Node, nodeIds: Set[ProcessId],
    s: PCounterLocalState): PCounterModule =
  {
    val newC = PCounterModule(parentNode, nodeIds)
    newC.s.pCounter = s.pCounter
    newC
  }
}

class PCounterModule(val parentNode: Node, nodeIds: Set[ProcessId])
  extends NodeModule
{
  val algoCode: AlgoCode = AlgoCodes.P_COUNTER

  ////////////////////
  //LOCAL STATE
  private object s extends PCounterLocalState {
    var pCounter: PCounter = PCounter(parentNode.id, nodeIds)
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
    PCounterModule.buildWith(parentNode, nodeIds, s)

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

  private def updatedPCounterResponse: Message = {
    val response = Response(UpdatedPCounter(s.pCounter))
    Message(response, parentNode.id, clock.stamp())
  }

  private def startGossip(): Unit = {
    val mergeCmd = MergePCounter(s.pCounter)
    val broadcastCmd = Broadcast(mergeCmd, parentNode.id, clock.stamp())
    val initiateBroadcastMsg = Message(broadcastCmd, parentNode.id,
      clock.stamp())
    parentNode.deliver(initiateBroadcastMsg)
  }
}
