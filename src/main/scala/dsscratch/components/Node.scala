package dsscratch.components

import dsscratch.modules._
import dsscratch.modules.snapshots.Snapshot
import dsscratch.modules.test._
import dsscratch.client_server._
import dsscratch.clocks._
import dsscratch.timers._
import dsscratch.util.Log
import dsscratch.util.MutableDeque
import dsscratch.util.Rand

import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

trait NodeLocalState extends LocalState {}

class NodeModuleParent(private val node: Node) extends ModuleParent {
  def id: ProcessId = node.id
  def clock: Clock = node.clock
  def log: Log = node.log
  def outProcessIds: ArrayBuffer[ProcessId] = node.outChs.flatMap(_.targetIds)
  def inProcessIds: ArrayBuffer[ProcessId] = node.inChs.flatMap(_.sourceIds)

  def deliver(cmd: Command): Unit = {
    node.deliver(Message(cmd, node.id, clock.stamp()))
  }

  def send(targetProcessId: ProcessId, cmd: Command): Unit = {
    node.send(targetProcessId, Message(cmd, node.id, clock.stamp()))
  }

  def takeSnapshot(snapId: TimeStamp): Unit = node.takeSnapshot(snapId)

  def setTimer(timeout: Int, f: () => Unit, repeat: Boolean = false): Timer = {
    node.setTimer(timeout, f, repeat)
  }

  def cancelTimer(t: Timer): Unit = node.cancelTimer(t)

  override def toString(): String = node.toString
}

object NodeModuleParent {
  def apply(node: Node): NodeModuleParent = new NodeModuleParent(node)
}

class Node(clk: Clock = EmptyClock(), val initiator: Boolean = false)
  extends Process
{
  var modules = ArrayBuffer[Module]()
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

  def addModule(c: Module) = {
    if (!modules.exists(_.moduleCode == c.moduleCode)) modules.append(c)
  }

  def connectClient(conn: ClientConnection): Unit = {
    modules.foreach(_.connectClient(conn))
  }

  def disconnectClient(conn: ClientConnection): Unit = {
    modules.foreach(_.disconnectClient(conn))
  }

  override def takeSnapshot(snapId: TimeStamp): Unit = {
    val snap = new Node(clock.snapshot, initiator)
    snap.modules = modules.map(_.snapshot)
    snap.initiated = initiated
    snap.snapshots = snapshots.clone()
    snapshots.append(Snapshot(snapId, snap))
  }

  def terminatedFor(moduleCode: ModuleCode): Boolean = {
    if (!modules.exists(_.moduleCode == moduleCode)) return false

    modules.filter(_.moduleCode == moduleCode).head.terminated
  }

  def resultFor(moduleCode: ModuleCode): String = {
    if (!modules.exists(_.moduleCode == moduleCode)) return ""

    modules.filter(_.moduleCode == moduleCode).head.result
  }

  def testCodeFor(moduleCode: ModuleCode): TestCode = {
    if (!modules.exists(_.moduleCode == moduleCode)) return TestCodes.NO_VAL

    modules.filter(_.moduleCode == moduleCode).head.testCode
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
