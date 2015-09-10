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
//There is no guarantee messages will be received in a consistent order.

trait SimpleBroadcastLocalState extends LocalState {
  var processedCommands: mLRUSet[Command]
}

object SimpleBroadcastComponent {
  def apply(parentProcess: Process): SimpleBroadcastComponent = {
    new SimpleBroadcastComponent(parentProcess)
  }
  def buildWith(parentProcess: Process, s: SimpleBroadcastLocalState): SimpleBroadcastComponent = {
    val newC = SimpleBroadcastComponent(parentProcess)

    newC.s.processedCommands = s.processedCommands
    newC
  }
}

class SimpleBroadcastComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.SIMPLE_BROADCAST
  val outChs = parentProcess.outChs
  val inChs = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends SimpleBroadcastLocalState {
    var processedCommands = mLRUSet[Command]()
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case Broadcast(cmd, sender, ts) => {
        val simpleBroadcast = SimpleBroadcast(cmd, sender, ts)
        parentProcess.recv(Message(simpleBroadcast, parentProcess, clock.stamp()))
      }
      case SimpleBroadcast(cmd, _, _) => {
        if (s.processedCommands.contains(cmd)) return
        val unwrapped = Message(cmd, m.sender, clock.stamp())
        if (m.sender != parentProcess) parentProcess.recv(unwrapped)
        s.processedCommands += cmd

        broadcastMessage(m.cmd)
      }
      case _ => // Ignore the rest
    }
  }

  def terminated: Boolean = false

  def terminatedFor(cmd: Command): Boolean = s.processedCommands.contains(cmd)

  def step(): Unit = {
    if (parentProcess.failed) return
    //nothing to do...
  }

  def broadcastMessage(cmd: Command): Unit = {
    val newBroadcast = Message(cmd, parentProcess, clock.stamp())
    for (ch <- outChs) ch.recv(newBroadcast)
  }

  def snapshot: SimpleBroadcastComponent = SimpleBroadcastComponent.buildWith(parentProcess, s)

  def result = ""
}
