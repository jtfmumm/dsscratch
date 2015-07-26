package dsscratch.components

import dsscratch.clocks.TimeStamp

trait Command


case class Token(id: Int)


case class ProcessToken(t: Token) extends Command

case class TakeSnapshot(snapId: TimeStamp) extends Command
