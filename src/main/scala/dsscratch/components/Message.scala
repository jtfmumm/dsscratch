package dsscratch.components

import dsscratch.clocks.{TS, TimeStamp}

case class Message(cmd: Command, sender: Process, ts: TimeStamp = TS()) {
  override def toString: String = "Message from " + sender + ": " + cmd + " at " + ts
}
