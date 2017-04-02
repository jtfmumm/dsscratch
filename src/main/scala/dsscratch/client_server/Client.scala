package dsscratch.client_server

import dsscratch.algos.nodes._
import dsscratch.clocks._
import dsscratch.components._

class Client(clk: Clock, val clientAlgo: ClientAlgorithm)
  extends Process {
  var conn: ClientConnection = EmptyClientConnection

  def recv(m: Message): Unit = {
    m.cmd match {
      case r: Response => clientAlgo.processResponse(r.cmd, m.senderId, m.ts)
    }
  }

  def connect(server: Node): Unit = {
    conn = new LiveClientConnection(this, server)
  }

  def disconnect(): Unit = {
    conn = EmptyClientConnection
  }

  def step(): Unit = {
    clientAlgo.run(this, conn)
  }

  def sendRequest(cmd: Command): Unit = {
    val message = Message(Request(cmd, conn), id, clk.stamp())
    conn.request(message)
  }
}
