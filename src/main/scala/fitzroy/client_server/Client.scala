package fitzroy.client_server

import fitzroy.clocks._
import fitzroy.components._

class Client(val clientLogic: ClientLogic, clk: Clock = EmptyClock())
  extends Process {
  var conn: ClientConnection = EmptyClientConnection

  override val clock = clk match {
    case EmptyClock() => LamportClock(code)
    case _ => clk
  }

  def recv(m: Message): Unit = {
    clientLogic.processResponse(m.cmd, m.senderId, m.ts)
  }

  def connect(server: Node): Unit = {
    conn = new LiveClientConnection(this, server)
  }

  def disconnect(): Unit = {
    conn = EmptyClientConnection
  }

  def step(): Unit = {
    clientLogic.run(this, conn)
  }

  def sendRequest(cmd: Command): Unit = {
    val message = Message(Request(cmd, conn), id, clk.stamp())
    conn.request(message)
  }

  // For testing
  def isLocallyCorrect: Boolean = clientLogic.isLocallyCorrect
  def value: Any = clientLogic.value
}

object Client {
  def apply(clientLogic: ClientLogic) = new Client(clientLogic)
}
