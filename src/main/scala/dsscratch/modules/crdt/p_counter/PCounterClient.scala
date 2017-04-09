package dsscratch.modules.crdt.p_counter

import dsscratch.client_server._
import dsscratch.clocks._
import dsscratch.components._
import dsscratch.datast.crdt._
import dsscratch.modules.crdt._
import dsscratch.util.Rand

class PCounterClientLogic extends ClientLogic {
  def run(parent: Client, conn: ClientConnection): Unit = {
    if (Rand.roll(6) < 2)
      parent.sendRequest(IncrementPCounter)
  }

  def processResponse(cmd: Command, senderId: ProcessId, ts: TimeStamp):
    Unit = {
    // Use for testing
  }

  def isLocallyCorrect: Boolean = true
  def value: Any = Nil
}
