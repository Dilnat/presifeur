package presifeur.server

import presifeur.engine.GameEngine
import presifeur.model.*
import zio.*

private case class LobbyEntry(name: String, queue: Queue[ServerMessage])

private enum RoomState:
  case Lobby(entries: Vector[LobbyEntry])
  case Playing(game: GameState, queues: Map[Int, Queue[ServerMessage]])
  case Done

class GameRoom private (stateRef: Ref[RoomState], val minPlayers: Int):

  // Rejoint le lobby ; retourne le playerId assigné, ou échec si la partie est déjà lancée.
  def join(name: String, queue: Queue[ServerMessage]): IO[String, Int] =
    stateRef.modify {
      case RoomState.Lobby(entries) =>
        val id      = entries.size
        val updated = entries :+ LobbyEntry(name, queue)
        if updated.size >= minPlayers then
          val names  = updated.map(_.name).toList
          val game   = GameEngine.newGame(names)
          val queues = updated.zipWithIndex.map((e, i) => i -> e.queue).toMap
          (Right((id, Some((game, queues)))), RoomState.Playing(game, queues))
        else
          (Right((id, None)), RoomState.Lobby(updated))
      case other =>
        (Left("La partie a déjà commencé."), other)
    }.flatMap {
      case Left(err)                       => ZIO.fail(err)
      case Right((id, Some((game, qs))))   => broadcastState(game, qs).as(id)
      case Right((id, None))               =>
        stateRef.get.flatMap {
          case RoomState.Lobby(entries) => broadcastWaiting(entries)
          case _                         => ZIO.unit
        }.as(id)
    }

  def play(playerId: Int, cardTokens: List[String]): UIO[Unit] =
    stateRef.modify {
      case s @ RoomState.Playing(game, queues) =>
        if game.currentPlayerIdx != playerId then
          (sendTo(queues, playerId, ServerMessage.Error("Ce n'est pas votre tour.")), s)
        else
          val parsed = cardTokens.flatMap(CardParser.parse)
          if parsed.size != cardTokens.size then
            (sendTo(queues, playerId, ServerMessage.Error("Cartes non reconnues.")), s)
          else GameEngine.applyPlay(game, parsed) match
            case Left(err)      =>
              (sendTo(queues, playerId, ServerMessage.Error(s"Coup invalide : $err")), s)
            case Right(newGame) =>
              val advanced = skipEmptyHands(newGame)
              if advanced.isGameOver then
                (endGame(advanced, queues), RoomState.Done)
              else
                (broadcastState(advanced, queues), RoomState.Playing(advanced, queues))
      case s => (ZIO.unit, s)
    }.flatten

  def pass(playerId: Int): UIO[Unit] =
    stateRef.modify {
      case s @ RoomState.Playing(game, queues) =>
        if game.currentPlayerIdx != playerId then
          (sendTo(queues, playerId, ServerMessage.Error("Ce n'est pas votre tour.")), s)
        else GameEngine.applyPass(game) match
          case Left(err)      =>
            (sendTo(queues, playerId, ServerMessage.Error(err)), s)
          case Right(newGame) =>
            val advanced = skipEmptyHands(newGame)
            if advanced.isGameOver then
              (endGame(advanced, queues), RoomState.Done)
            else
              (broadcastState(advanced, queues), RoomState.Playing(advanced, queues))
      case s => (ZIO.unit, s)
    }.flatten

  // Avance currentPlayerIdx en sautant les joueurs qui n'ont plus de cartes.
  @annotation.tailrec
  private def skipEmptyHands(game: GameState, limit: Int = 0): GameState =
    if limit >= game.players.size || game.isGameOver || game.currentPlayer.hasCards then game
    else skipEmptyHands(game.copy(currentPlayerIdx = game.nextPlayerIdx), limit + 1)

  private def endGame(game: GameState, queues: Map[Int, Queue[ServerMessage]]): UIO[Unit] =
    val ranked   = GameEngine.assignRoles(game.finishOrder, game.players)
    val rankings = ranked.map(p => RankingEntry(p.role.fold("?")(_.nom), p.name)).toList
    ZIO.foreachDiscard(queues.values)(_.offer(ServerMessage.GameOver(rankings)).unit)

  private def broadcastState(game: GameState, queues: Map[Int, Queue[ServerMessage]]): UIO[Unit] =
    ZIO.foreachDiscard(queues.toList) { (playerId, queue) =>
      val player = game.players(playerId)
      val msg = ServerMessage.State(
        hand          = player.hand.map(_.toString),
        table         = game.lastPlay.map(p => s"${p.rank.courte} x${p.size}"),
        currentPlayer = game.currentPlayer.name,
        isYourTurn    = game.currentPlayerIdx == playerId,
        players       = game.players.map(p =>
                          PlayerInfo(p.name, p.cardCount, p.id == game.currentPlayerIdx)
                        ).toList,
        round         = game.round
      )
      queue.offer(msg).unit
    }

  private def broadcastWaiting(entries: Vector[LobbyEntry]): UIO[Unit] =
    val msg = ServerMessage.Waiting(entries.map(_.name).toList, minPlayers - entries.size)
    ZIO.foreachDiscard(entries)(_.queue.offer(msg).unit)

  private def sendTo(queues: Map[Int, Queue[ServerMessage]], id: Int, msg: ServerMessage): UIO[Unit] =
    queues.get(id).fold(ZIO.unit)(_.offer(msg).unit)

object GameRoom:
  def make(minPlayers: Int): UIO[GameRoom] =
    Ref.make[RoomState](RoomState.Lobby(Vector.empty))
      .map(new GameRoom(_, minPlayers))
