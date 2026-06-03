package presifeur.server

import presifeur.engine.GameEngine
import presifeur.model.*
import zio.*

private case class LobbyEntry(id: Int, name: String, queue: Queue[ServerMessage])

private case class ActiveGame(
  game: GameState,
  queues: Map[Int, Queue[ServerMessage]],
  gameIdxByRoomId: Map[Int, Int]
)

private case class RoomState(
  lobby: Vector[LobbyEntry],
  masterId: Option[Int],
  active: Option[ActiveGame],
  nextId: Int
)

class GameRoom private (stateRef: Ref[RoomState], val minPlayers: Int):

  def join(name: String, queue: Queue[ServerMessage]): IO[String, Int] =
    stateRef.modify { state =>
      val entry        = LobbyEntry(state.nextId, name, queue)
      val updatedLobby = state.lobby :+ entry
      val updatedState = state.copy(
        lobby = updatedLobby,
        masterId = state.masterId.orElse(Some(entry.id)),
        nextId = state.nextId + 1
      )
      val masterName = updatedState.masterId.flatMap(id => updatedLobby.find(_.id == id).map(_.name)).getOrElse(name)
      val effect = state.active match
        case Some(_) =>
          queue.offer(waitingMessage(updatedLobby, masterName, updatedState.masterId, entry.id, isPlaying = true)).unit
        case None =>
          broadcastWaiting(updatedLobby, masterName, updatedState.masterId, isPlaying = false)
      val result: Either[String, (Int, UIO[Unit])] = Right((entry.id, effect))
      (result, updatedState)
    }.flatMap {
      case Left(err)          => ZIO.fail(err)
      case Right((id, effect)) => effect.as(id)
    }

  def start(playerId: Int): IO[String, Unit] =
    stateRef.modify { state =>
      if state.active.nonEmpty then
        (Left("La partie est déjà en cours."), state)
      else if state.masterId.forall(_ != playerId) then
        (Left("Seul le master peut lancer la partie."), state)
      else if state.lobby.size < minPlayers then
        (Left(s"Il faut au moins $minPlayers joueurs."), state)
      else
        val participants = state.lobby
        val game         = GameEngine.newGame(participants.map(_.name).toList)
        val queues       = participants.zipWithIndex.map((entry, index) => index -> entry.queue).toMap
        val roomMap      = participants.zipWithIndex.map((entry, index) => entry.id -> index).toMap
        val active       = ActiveGame(game, queues, roomMap)
        val updated      = state.copy(active = Some(active))
        val result: Either[String, UIO[Unit]] = Right(broadcastState(game, queues))
        (result, updated)
    }.flatMap {
      case Left(err)     => ZIO.fail(err)
      case Right(effect) => effect
    }

  def play(playerId: Int, cardTokens: List[String]): UIO[Unit] =
    stateRef.modify {
      case state @ RoomState(_, _, Some(active), _) =>
        val game   = active.game
        val queues = active.queues
        active.gameIdxByRoomId.get(playerId) match
          case None =>
            (sendTo(queues, playerId, ServerMessage.Error("Rejoignez d'abord la partie en cours.")), state)
          case Some(gamePlayerId) if game.currentPlayerIdx != gamePlayerId =>
            (sendTo(queues, playerId, ServerMessage.Error("Ce n'est pas votre tour.")), state)
          case Some(_) =>
            val parsed = cardTokens.flatMap(CardParser.parse)
            if parsed.size != cardTokens.size then
              (sendTo(queues, playerId, ServerMessage.Error("Cartes non reconnues.")), state)
            else GameEngine.applyPlay(game, parsed) match
              case Left(err)      =>
                (sendTo(queues, playerId, ServerMessage.Error(s"Coup invalide : $err")), state)
              case Right(newGame) =>
                val advanced = skipEmptyHands(newGame)
                if advanced.isGameOver then
                  val masterName = state.masterId.flatMap(id => state.lobby.find(_.id == id).map(_.name)).getOrElse("?")
                  val updated    = state.copy(active = None)
                  (
                    endGame(advanced, queues) *> broadcastWaiting(state.lobby, masterName, state.masterId, isPlaying = false),
                    updated
                  )
                else
                  val updatedActive = active.copy(game = advanced)
                  (broadcastState(advanced, queues), state.copy(active = Some(updatedActive)))
      case state => (ZIO.unit, state)
    }.flatten

  def pass(playerId: Int): UIO[Unit] =
    stateRef.modify {
      case state @ RoomState(_, _, Some(active), _) =>
        val game   = active.game
        val queues = active.queues
        active.gameIdxByRoomId.get(playerId) match
          case None =>
            (sendTo(queues, playerId, ServerMessage.Error("Rejoignez d'abord la partie en cours.")), state)
          case Some(gamePlayerId) if game.currentPlayerIdx != gamePlayerId =>
            (sendTo(queues, playerId, ServerMessage.Error("Ce n'est pas votre tour.")), state)
          case Some(_) =>
            GameEngine.applyPass(game) match
              case Left(err)      =>
                (sendTo(queues, playerId, ServerMessage.Error(err)), state)
              case Right(newGame) =>
                val advanced = skipEmptyHands(newGame)
                if advanced.isGameOver then
                  val masterName = state.masterId.flatMap(id => state.lobby.find(_.id == id).map(_.name)).getOrElse("?")
                  val updated    = state.copy(active = None)
                  (
                    endGame(advanced, queues) *> broadcastWaiting(state.lobby, masterName, state.masterId, isPlaying = false),
                    updated
                  )
                else
                  val updatedActive = active.copy(game = advanced)
                  (broadcastState(advanced, queues), state.copy(active = Some(updatedActive)))
      case state => (ZIO.unit, state)
    }.flatten

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
        tableCards    = game.lastPlay.map(_.cards.map(_.toString)).getOrElse(Nil),
        currentPlayer = game.currentPlayer.name,
        isYourTurn    = game.currentPlayerIdx == playerId,
        players       = game.players.map(p =>
                          PlayerInfo(p.name, p.cardCount, p.id == game.currentPlayerIdx)
                        ).toList,
        round         = game.round
      )
      queue.offer(msg).unit
    }

  private def broadcastWaiting(
    entries: Vector[LobbyEntry],
    masterName: String,
    masterId: Option[Int],
    isPlaying: Boolean
  ): UIO[Unit] =
    ZIO.foreachDiscard(entries) { entry =>
      entry.queue.offer(waitingMessage(entries, masterName, masterId, entry.id, isPlaying)).unit
    }

  private def waitingMessage(
    entries: Vector[LobbyEntry],
    masterName: String,
    masterId: Option[Int],
    recipientId: Int,
    isPlaying: Boolean
  ): ServerMessage.Waiting =
    val needed  = math.max(minPlayers - entries.size, 0)
    val isMaster = masterId.contains(recipientId)
    val canStart = !isPlaying && isMaster && entries.size >= minPlayers
    ServerMessage.Waiting(masterName, entries.map(_.name).toList, needed, isMaster, canStart, isPlaying)

  private def sendTo(queues: Map[Int, Queue[ServerMessage]], id: Int, msg: ServerMessage): UIO[Unit] =
    queues.get(id).fold(ZIO.unit)(_.offer(msg).unit)

object GameRoom:
  def make(minPlayers: Int): UIO[GameRoom] =
    Ref.make[RoomState](RoomState(Vector.empty, None, None, 0))
      .map(new GameRoom(_, minPlayers))