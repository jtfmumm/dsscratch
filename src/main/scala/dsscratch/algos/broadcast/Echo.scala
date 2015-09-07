package dsscratch.algos.broadcast

import dsscratch.algos.AlgoCode
import dsscratch.algos.AlgoCodes
import dsscratch.algos.nodes.LocalState
import dsscratch.algos.nodes.NodeComponent
import dsscratch.components.Message
import dsscratch.components.Process
import dsscratch.components._


import scala.collection.mutable.{Set => mSet}

// When a initiateEcho command is received, a node
// sends a message to its neighbors.

// When a node receives an Echo command, if it has no
// parent, it records the sender as its parent and delivers
// the contained message to itself. It then
// sends the message over all its channels except to its
// parent. It only sends to its parent once its received
// the same Echo over all incoming channels.

trait EchoLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var echoParent: Process
  var currentMessage: Option[Message]
  var chsToReceive: mSet[Channel]
}

object EchoComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): EchoComponent = {
    new EchoComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: EchoLocalState): EchoComponent = {
    val newC = EchoComponent(parentProcess, s.initiator)

    //Set state properties
    newC.s.initiated = s.initiated
    newC.s.echoParent = s.echoParent
    newC.s.currentMessage = s.currentMessage
    newC.s.chsToReceive = s.chsToReceive
    newC
  }
}

class EchoComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.ECHO
  val outChs = parentProcess.outChs
  val inChs = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends EchoLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var echoParent: Process = EmptyProcess
    var currentMessage: Option[Message] = None
    var chsToReceive: mSet[Channel] = mSet[Channel](inChs: _*)
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case i @ InitiateEcho(msg) => {
        s.initiator = true
        s.currentMessage = Some(Message(Echo(msg), parentProcess, m.ts))
      }
      case e @ Echo(msg) => if (!terminated) processEcho(m)
      case _ => // do nothing
    }
  }

  def terminated: Boolean = s.chsToReceive.isEmpty && s.currentMessage.isEmpty

  def step(): Unit = {
    if (parentProcess.failed) return
    if (terminated) return
    if (s.initiator && !s.initiated) initiate()
  }

  def snapshot: EchoComponent = EchoComponent.buildWith(parentProcess, s)

  def result = s.currentMessage.toString

  private def initiate(): Unit = s.currentMessage match {
    case Some(msg) => {
      for (c <- outChs) c.recv(msg)
      s.initiated = true
      s.echoParent = parentProcess
    }
    case None => s.initiated = true
  }

  private def processEcho(m: Message) = {
    s.chsToReceive -= inChs.filter(_.hasSource(m.sender)).head

    if (s.echoParent == EmptyProcess) {
      s.echoParent = m.sender
      s.currentMessage = Some(m)
      for (c <- outChs.filter(!_.hasTarget(m.sender))) c.recv(m)
      // Deliver wrapped message to self
      m.cmd match { case Echo(msg) => parentProcess.recv(msg) }
    } else {
      if (s.chsToReceive.isEmpty) outChs.filter(_.hasTarget(s.echoParent)).head.recv(m)
      s.currentMessage = None
      s.echoParent = EmptyProcess
    }
  }
}

