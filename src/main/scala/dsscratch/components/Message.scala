package dsscratch.components

case class Message(cmd: Command, sender: Process, ts: TimeStamp = Count(-1))
