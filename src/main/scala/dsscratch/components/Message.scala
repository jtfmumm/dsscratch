package dsscratch.components

import dsscratch.clocks.{Count, TimeStamp}

case class Message(cmd: Command, sender: Process, ts: TimeStamp = Count(-1))
