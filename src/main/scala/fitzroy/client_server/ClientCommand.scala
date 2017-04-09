package fitzroy.client_server

import fitzroy.components.Command

////Send a request from a client to a server
case class Request(cmd: Command, conn: ClientConnection) extends Command
