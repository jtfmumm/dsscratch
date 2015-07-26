package dsscratch.algos.tarry

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.algos._
import dsscratch.algos.nodes._
import randomific.Rand
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import dsscratch.util.Log
import dsscratch.runner.TopologyRunner
import dsscratch.draw.DotGraph
import scala.collection.mutable.{Map => mMap}
import scala.util.Random

//Chandy-Lamport snapshot algorithm
//Kicked off by any initiator (here we choose one)
//1) The initiator takes a local snapshot and sends control messages through all channels.
//2) When a process receives a control message for the first time, it takes a local
//   snapshot and sends control messages through all channels.
//3) A process p records the state of an incoming channel c by taking a snapshot of all incoming
//   messages M on c from the time p first received a control message to the time that a
//   control message is received over c (these can coincide, in which case M is empty)
//4) A process terminates when it has received control messages over all channels and has sent
//   control messages over all channels.


trait ChandyLamportLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var chsSent: ArrayBuffer[Channel]
  var chsReceived: ArrayBuffer[Channel]
  var controlMessages: ArrayBuffer[Command]
  var chStates: mMap[Int, ArrayBuffer[Message]]
}

object ChandyLamportComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): ChandyLamportComponent = {
    new ChandyLamportComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: ChandyLamportLocalState): ChandyLamportComponent = {
    val newC = ChandyLamportComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.chsSent = s.chsSent.map(x => x)
    newC.s.chsReceived = s.chsReceived.map(x => x)
    newC.s.controlMessages = s.controlMessages.map(x => x)
    newC.s.chStates = s.chStates.map(x => x)
    newC
  }
}

class ChandyLamportComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.CHANDY_LAMPORT
  val outChs = parentProcess.outChs
  val inChs = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends ChandyLamportLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var chsSent = ArrayBuffer[Channel]()
    var chsReceived = ArrayBuffer[Channel]()
    var controlMessages = ArrayBuffer[Command]()
    var chStates = {
      val cs = mMap[Int, ArrayBuffer[Message]]()
      inChs.foreach((ch: Channel) => cs.update(ch.id, ArrayBuffer[Message]()))
      cs
    }
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case c @ TakeSnapshot(_) => takeSnapshot(c, m.sender)
      case _ => {
        if (s.chsReceived.exists(_.hasSource(m.sender))) return
        val newCh = inChs.filter(_.hasSource(m.sender))(0)
        s.chStates(newCh.id).append(m)  // Add this message to the list of messages for our current inCh state
      }
    }
  }

  def terminated: Boolean = s.chsSent.size == outChs.size && s.chsReceived.size == inChs.size

  def step(): Unit = {
    if (parentProcess.failed) return
    if (s.initiator && !s.initiated) initiate()
    if (terminated) return
    if (outChs.size > s.chsSent.size) sendNextControlMessage()
  }

  def initiate(): Unit = {
    val cmd = TakeSnapshot(parentProcess.clock.stamp())
    s.controlMessages.append(cmd)
    val firstCh: Channel = Rand.pickItem(outChs)
    val msg = Message(cmd, parentProcess, clock.stamp())
    log.write("[CL] Sending on " + firstCh, msg.ts)
    firstCh.recv(msg)
    s.chsSent.append(firstCh)
    parentProcess.takeSnapshot(cmd.snapId)
    s.initiated = true
  }

  def snapshot: ChandyLamportComponent = ChandyLamportComponent.buildWith(parentProcess, s)

  def result = {
    val status = if (terminated) "Terminated" else "Not Terminated"
    parentProcess + ": " + status
  }

  private def takeSnapshot(cmd: TakeSnapshot, sender: Process): Unit = {
    if (s.chsReceived.exists(_.hasSource(sender))) return

    val newCh = inChs.filter(_.hasSource(sender))(0)
    s.chsReceived.append(newCh)

    if (!s.initiated) {
      s.controlMessages.append(cmd)
      parentProcess.takeSnapshot(cmd.snapId)
      s.initiated = true
    }
  }

  private def sendNextControlMessage() = {
    val chsLeft = outChs.filter(!s.chsSent.contains(_))
    val shuffled: ArrayBuffer[Channel] = Random.shuffle(chsLeft)
    val nextCh: Channel = shuffled(0)
    val cmd = s.controlMessages.last
    val msg = Message(cmd, parentProcess, clock.stamp())
    log.write("[CL] Sending on " + nextCh, msg.ts)
    nextCh.recv(msg)
  }
}

object ChandyLamportRunner {
  def runFor(nodeCount: Int, density: Double) = {}
}