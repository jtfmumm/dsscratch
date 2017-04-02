package dsscratch.client_server

import scala.collection.mutable.Queue
import dsscratch.algos.nodes._
import dsscratch.components._

trait ClientConnection extends Steppable {
  def request(m: Message)
  def response(m: Message)
}

class LiveClientConnection(val client: Client, val server: Node) extends ClientConnection {
  val requestMsgs = Queue[Message]()
  val responseMsgs = Queue[Message]()
  server.connectClient(this)

  def step(): Unit = {
    deliverNextRequest()
    deliverNextResponse()
  }

  def request(m: Message): Unit = {
    requestMsgs.enqueue(m)
  }

  def response(m: Message): Unit = {
    responseMsgs.enqueue(m)
  }

  private def deliverNextRequest(): Unit = {
    if (requestMsgs.isEmpty) return

    server.recv(requestMsgs.dequeue())
  }

  private def deliverNextResponse(): Unit = {
    if (responseMsgs.isEmpty) return

    client.recv(responseMsgs.dequeue())
  }
}

object EmptyClientConnection extends ClientConnection {
  def step(): Unit = {}
  def request(m: Message): Unit = {}
  def response(m: Message): Unit = {}
}
