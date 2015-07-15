package dsscratch.components

trait Process {
  def send(m: Message, ch: Channel): Unit = {
    ch.recv(m)
  }
  def recv(m: Message): Unit
  def step(): Unit
}
