package fitzroy.modules.crdt.p_counter

import fitzroy.client_server._
import fitzroy.clocks._
import fitzroy.components._
import fitzroy.datast.crdt._
import fitzroy.modules.crdt._
import fitzroy.util.Rand

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
