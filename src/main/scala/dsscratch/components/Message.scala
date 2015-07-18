package dsscratch.components

import dsscratch.clocks.{Count, TimeStamp}

case class Message(cmd: Command, sender: Process, ts: TimeStamp = Count(-1)) {
  override def toString: String = "Message from " + sender + ": " + ts + " " + cmd
}
