package dsscratch.client_server

import dsscratch.components.Command

////Send a request from a client to a server
case class Request(cmd: Command, conn: ClientConnection) extends Command
