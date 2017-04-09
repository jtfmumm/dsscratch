package dsscratch.client_server

import dsscratch.clocks._
import dsscratch.components._

trait ClientLogic {
  // Called whenever Client takes a step
  def run(parent: Client, conn: ClientConnection): Unit
  // Called when Client receives a response from a server
  def processResponse(cmd: Command, senderId: ProcessId, ts: TimeStamp): Unit

  ///////////
  // TESTING
  ///////////
  // Results look correct from local perspective
  def isLocallyCorrect: Boolean
  // Current value/result of processing
  def value: Any
}
