package dsscratch.client_server

import dsscratch.clocks._
import dsscratch.components._

trait ClientAlgorithm {
  def run(parent: Client, conn: ClientConnection): Unit
  def processResponse(cmd: Command, senderId: ProcessId, ts: TimeStamp): Unit
}
