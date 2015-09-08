package dsscratch.algos.broadcast

import dsscratch.algos.AlgoCode
import dsscratch.algos.AlgoCodes
import dsscratch.algos.nodes.LocalState
import dsscratch.algos.nodes.NodeComponent
import dsscratch.clocks.TimeStamp
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
  var currentCommand: Option[Command]
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
    newC.s.currentCommand = s.currentCommand
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
    var currentCommand: Option[Command] = None
    var chsToReceive: mSet[Channel] = mSet[Channel](inChs: _*)
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case i @ InitiateEcho(cmd, proc, ts) => {
        s.initiator = true
        s.currentCommand = Some(Echo(cmd, proc, ts))
      }
      case e @ Echo(cmd, _, _) => if (!terminated) processEcho(e, m.sender)
      case _ => // do nothing
    }
  }

  def terminated: Boolean = s.chsToReceive.isEmpty && s.currentCommand.isEmpty

  def step(): Unit = {
    if (parentProcess.failed) return
    if (terminated) return
    if (s.initiator && !s.initiated) initiate()
  }

  def snapshot: EchoComponent = EchoComponent.buildWith(parentProcess, s)

  def result = ""

  private def initiate(): Unit = s.currentCommand match {
    case Some(cmd) => {
      val newEcho = Message(cmd, parentProcess, clock.stamp())
      for (c <- outChs) c.recv(newEcho)
      s.initiated = true
      s.echoParent = parentProcess
    }
    case None => s.initiated = true
  }

  private def processEcho(cmd: Command, sender: Process) = {
    val newEcho = Message(cmd, parentProcess, clock.stamp())
    s.chsToReceive -= inChs.filter(_.hasSource(sender)).head

    if (s.echoParent == EmptyProcess) {
      s.echoParent = sender
      s.currentCommand = Some(cmd)
      for (c <- outChs.filter(!_.hasTarget(sender))) c.recv(newEcho)
      // Deliver command to self
      parentProcess.recv(newEcho)
    } else {
      if (s.chsToReceive.isEmpty) outChs.filter(_.hasTarget(s.echoParent)).head.recv(newEcho)
      s.currentCommand = None
      s.echoParent = EmptyProcess
    }
  }
}

