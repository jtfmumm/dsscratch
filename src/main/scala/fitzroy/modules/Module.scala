package fitzroy.modules

import fitzroy.modules.test._
import fitzroy.client_server._
import fitzroy.clocks._
import fitzroy.components._
import fitzroy.timers._
import fitzroy.util.Log
import scala.collection.mutable.ArrayBuffer

trait Module extends Steppable {
  // Each module should have a unique ModuleCode
  val moduleCode: ModuleCode
  // The interface to the process that contains this module
  val parent: ModuleParent

  val log: Log = parent.log
  val clock: Clock = parent.clock
  // The ProcessIds of processes from which we have incoming channels
  def inProcessIds: ArrayBuffer[ProcessId] = parent.inProcessIds
  // The ProcessIds of processes to which we have outgoing channels
  def outProcessIds: ArrayBuffer[ProcessId] = parent.outProcessIds

  ////////////////
  // Module logic
  ////////////////
  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit
  def snapshot: Module
  def terminated: Boolean

  ///////////
  // Testing
  ///////////
  // A String representation of current algorithm result
  def result: String
  // NO_VAL, SUCCESS, or FAILURE
  def testCode: TestCode = TestCodes.NO_VAL

  /////////////////
  // Client server
  /////////////////
  // Called when a client connects
  def connectClient(conn: ClientConnection): Unit = {}
  // Called when a client disconnects
  def disconnectClient(conn: ClientConnection): Unit = {}
}

trait ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): Module
}

trait ModuleParent {
  def id: ProcessId

  // Delivers command to all modules on this process
  def deliver(cmd: Command): Unit
  // Send cmd to process with targetProcessId (would lead to an exception
  // if there is no outgoing channel to that process)
  def send(targetProcessId: ProcessId, cmd: Command): Unit

  // Request that all modules snapshot their current state and this process
  // aggregates these into its own process snapshot
  def takeSnapshot(snapId: TimeStamp): Unit

  def setTimer(timeout: Int, f: () => Unit, repeat: Boolean = false): Timer
  def cancelTimer(t: Timer): Unit

  // Module has methods that call these methods (so they don't need to be
  // called directly in module code).
  def clock: Clock
  def log: Log
  def outProcessIds: ArrayBuffer[ProcessId]
  def inProcessIds: ArrayBuffer[ProcessId]
}
