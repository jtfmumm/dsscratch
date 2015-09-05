package dsscratch.algos.broadcast

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.components._
import dsscratch.util.mLRUSet


//If a node wants to broadcast a message, it sends it over every outgoing
//channel exactly once wrapped in a Broadcast command.
//
//Each other node sends the received broadcast command over every outgoing
//channel exactly once. Upon receiving it, the node delivers the contained
//message to itself.
//
//If a node or channel fails, the message may not reach every node.

trait SimpleBroadcastLocalState extends LocalState {
  var processedMessages: mLRUSet[Message]
}

object SimpleBroadcastComponent {
  def apply(parentProcess: Process): SimpleBroadcastComponent = {
    new SimpleBroadcastComponent(parentProcess)
  }
  def buildWith(parentProcess: Process, s: SimpleBroadcastLocalState): SimpleBroadcastComponent = {
    val newC = SimpleBroadcastComponent(parentProcess)

    newC.s.processedMessages = s.processedMessages
    newC
  }
}

class SimpleBroadcastComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.NONE // <-- use code
  val outChs = parentProcess.outChs
  val inChs = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends SimpleBroadcastLocalState {
    var processedMessages = mLRUSet[Message]()
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case Broadcast(msg) => {
        if (s.processedMessages.contains(m)) return
        if (m.sender != parentProcess) parentProcess.recv(msg)

        broadcastMessage(m)
      }
      case _ => // Ignore the rest
    }
  }

  def terminated: Boolean = false

  def step(): Unit = {
    if (parentProcess.failed) return
    //nothing to do...
  }

  def broadcastMessage(m: Message): Unit = {
    for (ch <- outChs) ch.recv(m)
    s.processedMessages += m
  }

  def snapshot: SimpleBroadcastComponent = SimpleBroadcastComponent.buildWith(parentProcess, s)

  def result = ""
}
