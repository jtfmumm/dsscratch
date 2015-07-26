package dsscratch.components

trait Command


case class Token(id: Int)

case class ProcessToken(t: Token) extends Command
