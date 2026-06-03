package presifeur.server

import zio.*
import zio.http.*
import zio.http.ChannelEvent.*
import zio.json.*
import zio.stream.ZStream

object GameServer:

  def routes(room: GameRoom): Routes[Any, Nothing] =
    Routes(
      Method.GET / "game" -> Handler.fromFunctionZIO[Request](_ => wsHandler(room).toResponse)
    )

  private def wsHandler(room: GameRoom): WebSocketApp[Any] =
    Handler.webSocket { channel =>
      for
        out         <- Queue.unbounded[ServerMessage]
        playerIdRef <- Ref.make(Option.empty[Int])
        sender      <- ZStream.fromQueue(out)
                         .mapZIO(msg => channel.send(Read(WebSocketFrame.text(msg.toJson))))
                         .runDrain
                         .catchAll(_ => ZIO.unit)
                         .fork
        _           <- receiveLoop(channel, out, playerIdRef, room)
        _           <- sender.interrupt
      yield ()
    }

  private def receiveLoop(
    channel: WebSocketChannel,
    out: Queue[ServerMessage],
    pidRef: Ref[Option[Int]],
    room: GameRoom
  ): UIO[Unit] =
    channel.receive.flatMap {
      case Read(WebSocketFrame.Text(json)) =>
        (json.fromJson[ClientMessage] match
          case Left(_)    => out.offer(ServerMessage.Error("Message invalide.")).unit
          case Right(msg) => handleMsg(msg, out, pidRef, room)
        ) *> receiveLoop(channel, out, pidRef, room)
      case Read(WebSocketFrame.Close(_, _)) => ZIO.unit
      case _                                => receiveLoop(channel, out, pidRef, room)
    }.catchAll(_ => ZIO.unit)

  private def handleMsg(
    msg: ClientMessage,
    out: Queue[ServerMessage],
    pidRef: Ref[Option[Int]],
    room: GameRoom
  ): UIO[Unit] =
    msg match
      case ClientMessage.Join(name) =>
        pidRef.get.flatMap {
          case Some(_) => out.offer(ServerMessage.Error("Vous êtes déjà dans la partie.")).unit
          case None    =>
            room.join(name, out)
              .flatMap(id => pidRef.set(Some(id)))
              .catchAll(err => out.offer(ServerMessage.Error(err)).unit)
        }
      case ClientMessage.Start() =>
        pidRef.get.flatMap {
          case None     => out.offer(ServerMessage.Error("Rejoignez d'abord la partie.")).unit
          case Some(id) => room.start(id).catchAll(err => out.offer(ServerMessage.Error(err)).unit)
        }
      case ClientMessage.Play(cards) =>
        pidRef.get.flatMap {
          case None     => out.offer(ServerMessage.Error("Rejoignez d'abord la partie.")).unit
          case Some(id) => room.play(id, cards)
        }
      case ClientMessage.Pass() =>
        pidRef.get.flatMap {
          case None     => out.offer(ServerMessage.Error("Rejoignez d'abord la partie.")).unit
          case Some(id) => room.pass(id)
        }
      case ClientMessage.Exchange(cards) =>
        pidRef.get.flatMap {
          case None     => out.offer(ServerMessage.Error("Rejoignez d'abord la partie.")).unit
          case Some(id) => room.exchange(id, cards)
        }
