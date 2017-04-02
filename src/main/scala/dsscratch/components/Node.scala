package dsscratch.components

import dsscratch.algos._
import dsscratch.algos.snapshots.Snapshot
import dsscratch.algos.test._
import dsscratch.client_server._
import dsscratch.clocks._
import dsscratch.timers._
import dsscratch.util.MutableDeque
import dsscratch.util.Rand

import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

trait NodeLocalState extends LocalState {}

class Node(clk: Clock = EmptyClock(), val initiator: Boolean = false)
  extends Process
{
  var modules = ArrayBuffer[NodeModule]()
  var snapshots = ArrayBuffer[Snapshot]()
  var outgoingQueue = Queue[(ProcessId, Message)]()
  var incomingQueue = Queue[Message]()
  var timers = Timers()

  override val clock = clk match {
    case EmptyClock() => LamportClock(code)
    case _ => clk
  }

  var initiated = false
  log.write(this + " log", clock.stamp())

  val outChs = ArrayBuffer[Channel]()
  val inChs = ArrayBuffer[Channel]()

  def outProcessIds: ArrayBuffer[ProcessId] = outChs.flatMap(_.targetIds)
  def inProcessIds: ArrayBuffer[ProcessId] = inChs.flatMap(_.sourceIds)

  def recv(m: Message): Unit = {
    if (failed) return
    clock.compareAndUpdate(m.ts)
    log.write("Receiving " + m, clock.stamp())
    incomingQueue.enqueue(m)
  }

  def deliver(m: Message): Unit = {
    m match {
      case Message(cmd, senderId, ts) =>
        modules.foreach(_.processMessage(cmd, senderId, ts))
    }
  }

  def send(targetProcessId: ProcessId, m: Message): Unit = {
    outgoingQueue.enqueue((targetProcessId, m))
  }

  def step(): Unit = {
    if (failed) return
    if (initiator && !initiated) initiate()
    processNextMessage()
    modules.foreach(_.step())
    sendNextMessage()
    timers.tick()
  }

  def setTimer(timeout: Int, f: () => Unit, repeat: Boolean = false): Timer =
  {
    val t = Timer(timeout, f, repeat)
    timers.addTimer(t)
    t
  }

  def cancelTimer(t: Timer): Unit = timers.removeTimer(t)

  private def initiate(): Unit = {}

  private def processNextMessage(): Unit = {
    if (incomingQueue.nonEmpty)
      incomingQueue.dequeue() match {
        case Message(cmd, senderId, ts) => {
          modules.foreach(_.processMessage(cmd, senderId, ts))
        }
      }
  }

  private def sendNextMessage(): Unit = {
    if (outgoingQueue.nonEmpty) {
      val (targetId, msg) = outgoingQueue.dequeue()
      outChs.filter(_.hasTargetId(targetId)).head.recv(msg)
    }
  }

  def addModule(c: NodeModule) = {
    if (!modules.exists(_.algoCode == c.algoCode)) modules.append(c)
  }

  def connectClient(conn: ClientConnection): Unit = {
    modules.foreach(_.connectClient(conn))
  }

  def disconnectClient(conn: ClientConnection): Unit = {
    modules.foreach(_.disconnectClient(conn))
  }

  override def takeSnapshot(snapId: TimeStamp) = {
    val snap = new Node(clock.snapshot, initiator)
    snap.modules = modules.map(_.snapshot)
    snap.initiated = initiated
    snap.snapshots = snapshots.clone()
    snapshots.append(Snapshot(snapId, snap))
  }

  def terminatedFor(algoCode: AlgoCode): Boolean = {
    if (!modules.exists(_.algoCode == algoCode)) return false

    modules.filter(_.algoCode == algoCode).head.terminated
  }

  def resultFor(algoCode: AlgoCode): String = {
    if (!modules.exists(_.algoCode == algoCode)) return ""

    modules.filter(_.algoCode == algoCode).head.result
  }

  def testCodeFor(algoCode: AlgoCode): TestCode = {
    if (!modules.exists(_.algoCode == algoCode)) return TestCodes.NO_VAL

    modules.filter(_.algoCode == algoCode).head.testCode
  }

  def addOutChannel(ch: Channel): Unit = {
    if (!outChs.contains(ch)) outChs.append(ch)
  }
  def removeOutChannel(ch: Channel): Unit = {
    if (!outChs.contains(ch)) return
    val i = outChs.indexOf(ch)
    outChs.remove(i)
  }
  def addInChannel(ch: Channel): Unit = {
    if (!inChs.contains(ch)) inChs.append(ch)
  }
  def removeInChannel(ch: Channel): Unit = {
    if (!inChs.contains(ch)) return
    val i = inChs.indexOf(ch)
    inChs.remove(i)
  }

  override def toString: String = "Node" + id
}

object Node {
  def apply(initiator: Boolean = false): Node =
    new Node(initiator = initiator)
}
