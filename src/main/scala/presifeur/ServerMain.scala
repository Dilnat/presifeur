package presifeur

import presifeur.server.{GameRoom, GameServer}
import zio.*
import zio.http.*

object ServerMain extends ZIOAppDefault:

  override def run: Task[Unit] =
    for
      room <- GameRoom.make(minPlayers = 3)
      _    <- Console.printLine("Serveur démarré sur ws://localhost:8080/game")
      _    <- Console.printLine("En attente de 3 joueurs...")
      _    <- Server.serve(GameServer.routes(room))
                .provide(Server.defaultWithPort(8080))
    yield ()
