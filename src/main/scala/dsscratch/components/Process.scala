package dsscratch.components


trait Process {
  def send(m: Message, ch: Channel): Unit = {
    ch.recv(m)
  }
  def recv(m: Message): Unit
  def step(): Unit
  def addChannel(ch: Channel): Unit
  def removeChannel(ch: Channel): Unit
}

case class EmptyProcess extends Process {
  val chs = List[Channel]()
  def recv(m: Message): Unit = {}
  def step(): Unit = {}
  def addChannel(ch: Channel): Unit = {}
  def removeChannel(ch: Channel): Unit = {}
}
