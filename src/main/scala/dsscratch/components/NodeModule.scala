package dsscratch.components

import dsscratch.algos._
import dsscratch.algos.test._
import dsscratch.client_server._
import dsscratch.clocks._

trait NodeModule extends Steppable {
  val parentNode: Node
  val algoCode: AlgoCode

  val log = parentNode.log
  val clock = parentNode.clock
  def inProcessIds = parentNode.inProcessIds
  def outProcessIds = parentNode.outProcessIds

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit
  def snapshot: NodeModule
  def terminated: Boolean

  // Testing
  def result: String
  def testCode: TestCode = TestCodes.NO_VAL

  // Client server
  def connectClient(conn: ClientConnection): Unit = {}
  def disconnectClient(conn: ClientConnection): Unit = {}
}

trait NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): NodeModule
}
