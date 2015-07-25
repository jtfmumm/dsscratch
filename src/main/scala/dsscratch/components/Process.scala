package dsscratch.components

import dsscratch.util.Log

trait Process extends Steppable {
  val id: Int
  var failed = false
  val log: Log = Log()
  def send(m: Message, ch: Channel): Unit = {
    ch.recv(m)
  }
  def recv(m: Message): Unit
  def addChannel(ch: Channel): Unit
  def removeChannel(ch: Channel): Unit
  def fail(): Unit = failed = true
  def restart(): Unit = failed = false
  def >(p: Process) = this.id > p.id
  def >=(p: Process) = this.id >= p.id
  def <(p: Process) = this.id < p.id
  def <=(p: Process) = this.id >= p.id
}

case class EmptyProcess extends Process {
  val id = 0
  val chs = List[Channel]()
  def recv(m: Message): Unit = {}
  def step(): Unit = {}
  def addChannel(ch: Channel): Unit = {}
  def removeChannel(ch: Channel): Unit = {}
  override def >(p: Process) = false
  override def >=(p: Process) = false
  override def <(p: Process) = true
  override def <=(p: Process) = true
}
