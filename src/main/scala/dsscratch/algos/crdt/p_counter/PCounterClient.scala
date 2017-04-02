package dsscratch.algos.crdt.p_counter

import dsscratch.client_server._
import dsscratch.clocks._
import dsscratch.components._
import dsscratch.util.Rand

class PCounterClientAlgorithm extends ClientAlgorithm {
  def run(parent: Client, conn: ClientConnection): Unit = {
    if (Rand.roll(6) < 2)
      parent.sendRequest(IncrementPCounter)
  }

  def processResponse(cmd: Command, senderId: ProcessId, ts: TimeStamp):
    Unit = {
    // Use for testing
  }
}
