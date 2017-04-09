package fitzroy.client_server

import scala.collection.mutable.Queue
import fitzroy.clocks._
import fitzroy.components._

class Response(cmd: Command, senderId: ProcessId, ts: TimeStamp) {
  def toMessage: Message = Message(cmd, senderId, ts)
}

object Response {
  def apply(cmd: Command, senderId: ProcessId, ts: TimeStamp) = {
    new Response(cmd, senderId, ts)
  }
}

trait ClientConnection extends Steppable {
  def request(req: Message)
  def response(res: Response)
}

class LiveClientConnection(val client: Client, val server: Node)
  extends ClientConnection {
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

  def response(res: Response): Unit = {
    responseMsgs.enqueue(res.toMessage)
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
  def response(m: Response): Unit = {}
}
