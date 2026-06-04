package presifeur.server

import presifeur.engine.GameEngine
import presifeur.model.*
import zio.*

private case class LobbyEntry(
    id: Int,
    name: String,
    queue: Queue[ServerMessage]
)

private case class ActiveGame(
    game: GameState,
    queues: Map[Int, Queue[ServerMessage]],
    gameIdxByRoomId: Map[Int, Int],
    exchange: Option[ExchangeState] = None,
    isFinished: Boolean = false,
    rankings: List[RankingEntry] = Nil,
    rankedPlayers: Vector[Player] = Vector.empty
)

private case class ExchangeState(
    presidentId: Int,
    trouducId: Int,
    presidentCards: Option[List[Card]] = None,
    trouducCards: Option[List[Card]] = None,
    vpId: Option[Int] = None,
    vtId: Option[Int] = None,
    vpCards: Option[List[Card]] = None,
    vtCards: Option[List[Card]] = None
)

private case class RoomState(
    lobby: Vector[LobbyEntry],
    masterId: Option[Int],
    active: Option[ActiveGame],
    nextId: Int
)

class GameRoom private (stateRef: Ref[RoomState], val minPlayers: Int):

  def join(name: String, queue: Queue[ServerMessage]): IO[String, Int] =
    stateRef
      .modify { state =>
        val entry = LobbyEntry(state.nextId, name, queue)
        val updatedLobby = state.lobby :+ entry
        val updatedState = state.copy(
          lobby = updatedLobby,
          masterId = state.masterId.orElse(Some(entry.id)),
          nextId = state.nextId + 1
        )
        val masterName = updatedState.masterId
          .flatMap(id => updatedLobby.find(_.id == id).map(_.name))
          .getOrElse(name)
        val effect = state.active match
          case Some(_) =>
            queue
              .offer(
                waitingMessage(
                  updatedLobby,
                  masterName,
                  updatedState.masterId,
                  entry.id,
                  isPlaying = true
                )
              )
              .unit
          case None =>
            broadcastWaiting(
              updatedLobby,
              masterName,
              updatedState.masterId,
              isPlaying = false
            )
        val result: Either[String, (Int, UIO[Unit])] = Right((entry.id, effect))
        (result, updatedState)
      }
      .flatMap {
        case Left(err)           => ZIO.fail(err)
        case Right((id, effect)) => effect.as(id)
      }

  def start(playerId: Int): IO[String, Unit] =
    stateRef
      .modify { state =>
        if state.masterId.forall(_ != playerId) then
          (Left("Seul le master peut lancer la partie."), state)
        else if state.lobby.size < minPlayers then
          (Left(s"Il faut au moins $minPlayers joueurs."), state)
        else
          state.active match
            case Some(active) =>
              if !active.isFinished then
                (Left("La partie est déjà en cours."), state)
              else
                // Next game starting!
                val participants = state.lobby
                val baseGame =
                  GameEngine.newGame(participants.map(_.name).toList)
                val queues = participants.zipWithIndex
                  .map((entry, index) => index -> entry.queue)
                  .toMap
                val roomMap = participants.zipWithIndex
                  .map((entry, index) => entry.id -> index)
                  .toMap

                // Carry over roles from active.rankedPlayers
                val rolesByName = active.rankedPlayers
                  .flatMap(p => p.role.map(p.name -> _))
                  .toMap
                val playersWithRoles = baseGame.players
                  .map(p => p.copy(role = rolesByName.get(p.name)))

                // Identify President, Trouduc, Vice-President, and Vice-Trouduc for exchange
                val president =
                  playersWithRoles.find(_.role.contains(Role.President))
                val trouduc =
                  playersWithRoles.find(_.role.contains(Role.Trouduc))
                val vp =
                  playersWithRoles.find(_.role.contains(Role.VicePresident))
                val vt =
                  playersWithRoles.find(_.role.contains(Role.ViceTrouduc))

                val (updatedGame, exchangeStateOpt) =
                  (president, trouduc) match {
                    case (Some(p), Some(t)) =>
                      val bestCards =
                        t.hand.sortBy(_.rank.value).reverse.take(2)
                      val vtBestCards = for {
                        vtPlayer <- vt
                        vpPlayer <- vp
                      } yield vtPlayer.hand.sortBy(_.rank.value).reverse.take(1)

                      val exState = ExchangeState(
                        presidentId = p.id,
                        trouducId = t.id,
                        presidentCards = None,
                        trouducCards = Some(bestCards),
                        vpId = vp.map(_.id),
                        vtId = vt.map(_.id),
                        vpCards = None,
                        vtCards = vtBestCards
                      )
                      (
                        baseGame.copy(
                          players = playersWithRoles,
                          isExchangePhase = true
                        ),
                        Some(exState)
                      )
                    case _ =>
                      (baseGame.copy(players = playersWithRoles), None)
                  }

                val newActive = ActiveGame(
                  game = updatedGame,
                  queues = queues,
                  gameIdxByRoomId = roomMap,
                  exchange = exchangeStateOpt,
                  isFinished = false
                )

                val effect =
                  broadcastState(updatedGame, queues, exchangeStateOpt)
                (Right(effect), state.copy(active = Some(newActive)))

            case None =>
              val participants = state.lobby
              val game = GameEngine.newGame(participants.map(_.name).toList)
              val queues = participants.zipWithIndex
                .map((entry, index) => index -> entry.queue)
                .toMap
              val roomMap = participants.zipWithIndex
                .map((entry, index) => entry.id -> index)
                .toMap
              val active = ActiveGame(game, queues, roomMap)
              val updated = state.copy(active = Some(active))
              val effect = broadcastState(game, queues)
              (Right(effect), updated)
      }
      .flatMap {
        case Left(err)     => ZIO.fail(err)
        case Right(effect) => effect
      }

  def play(playerId: Int, cardTokens: List[String]): UIO[Unit] =
    stateRef.modify {
      case state @ RoomState(_, _, Some(active), _) =>
        val game = active.game
        val queues = active.queues
        active.gameIdxByRoomId.get(playerId) match
          case None =>
            (
              sendTo(
                state.lobby,
                playerId,
                ServerMessage.Error("Rejoignez d'abord la partie en cours.")
              ),
              state
            )
          case Some(gamePlayerId) if game.currentPlayerIdx != gamePlayerId =>
            (
              sendTo(
                state.lobby,
                playerId,
                ServerMessage.Error("Ce n'est pas votre tour.")
              ),
              state
            )
          case Some(_) =>
            val parsed = cardTokens.flatMap(CardParser.parse)
            if parsed.size != cardTokens.size then
              (
                sendTo(
                  state.lobby,
                  playerId,
                  ServerMessage.Error("Cartes non reconnues.")
                ),
                state
              )
            else
              GameEngine.applyPlay(game, parsed) match
                case Left(err) =>
                  (
                    sendTo(
                      state.lobby,
                      playerId,
                      ServerMessage.Error(s"Coup invalide : $err")
                    ),
                    state
                  )
                case Right(newGame) =>
                  val advanced = skipEmptyHands(newGame)
                  if advanced.isGameOver then
                    val remaining = advanced.activePlayers
                    val lastIds = remaining
                      .map(_.id)
                      .filterNot(advanced.finishOrder.contains)
                    val finalOrder = advanced.finishOrder ++ lastIds
                    val rankedPlayers = GameEngine.assignRoles(
                      finalOrder,
                      advanced.players,
                      advanced.autoTrouduc
                    )
                    val effectiveOrder = finalOrder.filterNot(
                      advanced.autoTrouduc.contains
                    ) ++ advanced.autoTrouduc
                    val rankings = rankedPlayers
                      .sortBy(p => effectiveOrder.indexOf(p.id))
                      .map(p => RankingEntry(p.role.fold("?")(_.nom), p.name))
                      .toList

                    val finishedActive = active.copy(
                      game = advanced,
                      isFinished = true,
                      rankings = rankings,
                      rankedPlayers = rankedPlayers
                    )

                    val effect = ZIO.foreachDiscard(active.queues.toList) {
                      (gameIdx, queue) =>
                        val roomPlayerId =
                          active.gameIdxByRoomId.find(_._2 == gameIdx).map(_._1)
                        val isMaster = roomPlayerId
                          .flatMap(id => state.masterId.map(_ == id))
                          .getOrElse(false)
                        queue
                          .offer(ServerMessage.GameOver(rankings, isMaster))
                          .unit
                    }
                    (effect, state.copy(active = Some(finishedActive)))
                  else
                    val updatedActive = active.copy(game = advanced)
                    (
                      broadcastState(advanced, queues),
                      state.copy(active = Some(updatedActive))
                    )
      case state => (ZIO.unit, state)
    }.flatten

  def pass(playerId: Int): UIO[Unit] =
    stateRef.modify {
      case state @ RoomState(_, _, Some(active), _) =>
        val game = active.game
        val queues = active.queues
        active.gameIdxByRoomId.get(playerId) match
          case None =>
            (
              sendTo(
                state.lobby,
                playerId,
                ServerMessage.Error("Rejoignez d'abord la partie en cours.")
              ),
              state
            )
          case Some(gamePlayerId) if game.currentPlayerIdx != gamePlayerId =>
            (
              sendTo(
                state.lobby,
                playerId,
                ServerMessage.Error("Ce n'est pas votre tour.")
              ),
              state
            )
          case Some(_) =>
            GameEngine.applyPass(game) match
              case Left(err) =>
                (sendTo(state.lobby, playerId, ServerMessage.Error(err)), state)
              case Right(newGame) =>
                val advanced = skipEmptyHands(newGame)
                if advanced.isGameOver then
                  val remaining = advanced.activePlayers
                  val lastIds =
                    remaining.map(_.id).filterNot(advanced.finishOrder.contains)
                  val finalOrder = advanced.finishOrder ++ lastIds
                  val rankedPlayers = GameEngine.assignRoles(
                    finalOrder,
                    advanced.players,
                    advanced.autoTrouduc
                  )
                  val effectiveOrder = finalOrder.filterNot(
                    advanced.autoTrouduc.contains
                  ) ++ advanced.autoTrouduc
                  val rankings = rankedPlayers
                    .sortBy(p => effectiveOrder.indexOf(p.id))
                    .map(p => RankingEntry(p.role.fold("?")(_.nom), p.name))
                    .toList

                  val finishedActive = active.copy(
                    game = advanced,
                    isFinished = true,
                    rankings = rankings,
                    rankedPlayers = rankedPlayers
                  )

                  val effect = ZIO.foreachDiscard(active.queues.toList) {
                    (gameIdx, queue) =>
                      val roomPlayerId =
                        active.gameIdxByRoomId.find(_._2 == gameIdx).map(_._1)
                      val isMaster = roomPlayerId
                        .flatMap(id => state.masterId.map(_ == id))
                        .getOrElse(false)
                      queue
                        .offer(ServerMessage.GameOver(rankings, isMaster))
                        .unit
                  }
                  (effect, state.copy(active = Some(finishedActive)))
                else
                  val updatedActive = active.copy(game = advanced)
                  (
                    broadcastState(advanced, queues),
                    state.copy(active = Some(updatedActive))
                  )
      case state => (ZIO.unit, state)
    }.flatten

  def exchange(playerId: Int, cards: List[String]): UIO[Unit] =
    stateRef.modify {
      case state @ RoomState(_, _, Some(active), _)
          if active.exchange.isDefined =>
        val ex = active.exchange.get
        val parsed = cards.flatMap(CardParser.parse)

        active.gameIdxByRoomId.get(playerId) match
          case None =>
            (
              sendTo(
                state.lobby,
                playerId,
                ServerMessage.Error("Vous n'êtes pas dans la partie.")
              ),
              state
            )
          case Some(gamePlayerId) =>
            val player = active.game.players(gamePlayerId)
            val isPresident = gamePlayerId == ex.presidentId
            val isVP = ex.vpId.contains(gamePlayerId)
            val expectedCount = if isPresident then 2 else if isVP then 1 else 0

            if expectedCount == 0 then
              (
                sendTo(
                  state.lobby,
                  playerId,
                  ServerMessage.Error("Vous n'êtes pas autorisé à choisir des cartes pour l'échange.")
                ),
                state
              )
            else if parsed.size != expectedCount then
              (
                sendTo(
                  state.lobby,
                  playerId,
                  ServerMessage.Error(s"Vous devez choisir exactement $expectedCount carte(s).")
                ),
                state
              )
            else if !parsed.forall(player.hand.contains) then
              (
                sendTo(
                  state.lobby,
                  playerId,
                  ServerMessage.Error("Vous ne possédez pas ces cartes.")
                ),
                state
              )
            else
              val updatedEx = if isPresident then
                ex.copy(presidentCards = Some(parsed))
              else
                ex.copy(vpCards = Some(parsed))

              val isPresidentDone = updatedEx.presidentCards.isDefined && updatedEx.trouducCards.isDefined
              val isVPDone = updatedEx.vpId.forall(_ => updatedEx.vpCards.isDefined && updatedEx.vtCards.isDefined)

              if isPresidentDone && isVPDone then
                // Perform President/Trouduc swap
                val pIdx = ex.presidentId
                val tIdx = ex.trouducId
                val pCards = updatedEx.presidentCards.get
                val tCards = updatedEx.trouducCards.get

                var playersList = active.game.players
                val pPlayer = playersList(pIdx)
                val tPlayer = playersList(tIdx)

                val newPHand = (pPlayer.hand.filterNot(pCards.contains) ++ tCards).sortBy(_.rank.value)
                val newTHand = (tPlayer.hand.filterNot(tCards.contains) ++ pCards).sortBy(_.rank.value)

                playersList = playersList
                  .updated(pIdx, pPlayer.copy(hand = newPHand))
                  .updated(tIdx, tPlayer.copy(hand = newTHand))

                // Perform VP/VT swap if they exist
                for {
                  vpId <- ex.vpId
                  vtId <- ex.vtId
                  vpCards <- updatedEx.vpCards
                  vtCards <- updatedEx.vtCards
                } {
                  val vpPlayer = playersList(vpId)
                  val vtPlayer = playersList(vtId)
                  val newVpHand = (vpPlayer.hand.filterNot(vpCards.contains) ++ vtCards).sortBy(_.rank.value)
                  val newVtHand = (vtPlayer.hand.filterNot(vtCards.contains) ++ vpCards).sortBy(_.rank.value)
                  playersList = playersList
                    .updated(vpId, vpPlayer.copy(hand = newVpHand))
                    .updated(vtId, vtPlayer.copy(hand = newVtHand))
                }

                val firstIdx = playersList.indexWhere(
                  _.hand.exists(c =>
                    c.rank == Rank.Trois && c.suit == Suit.Trefles
                  )
                )
                val startingPlayerIdx =
                  if firstIdx >= 0 then firstIdx
                  else active.game.currentPlayerIdx

                val newGame = active.game.copy(
                  players = playersList,
                  currentPlayerIdx = startingPlayerIdx,
                  isExchangePhase = false
                )

                val effect = broadcastState(newGame, active.queues, None)
                (
                  effect,
                  state.copy(active =
                    Some(active.copy(game = newGame, exchange = None))
                  )
                )
              else
                val effect =
                  broadcastState(active.game, active.queues, Some(updatedEx))
                (
                  effect,
                  state.copy(active =
                    Some(active.copy(exchange = Some(updatedEx)))
                  )
                )
      case state => (ZIO.unit, state)
    }.flatten

  @annotation.tailrec
  private def skipEmptyHands(game: GameState, limit: Int = 0): GameState =
    if limit >= game.players.size || game.isGameOver || game.currentPlayer.hasCards
    then game
    else
      skipEmptyHands(
        game.copy(currentPlayerIdx = game.nextPlayerIdx),
        limit + 1
      )

  private def broadcastState(
      game: GameState,
      queues: Map[Int, Queue[ServerMessage]],
      exchange: Option[ExchangeState] = None
  ): UIO[Unit] =
    ZIO.foreachDiscard(queues.toList) { (playerId, queue) =>
      exchange match
        case Some(ex) =>
          val role = game.players
            .lift(playerId)
            .flatMap(_.role)
            .fold("Spectateur")(_.nom)
          val target =
            if playerId == ex.presidentId then "Trouduc"
            else if playerId == ex.trouducId then "Président"
            else if ex.vpId.contains(playerId) then "Vice-Trouduc"
            else if ex.vtId.contains(playerId) then "Vice-Président"
            else ""
          val count =
            if playerId == ex.presidentId then 2
            else if ex.vpId.contains(playerId) then 1
            else 0
          val isYourTurn =
            if playerId == ex.presidentId then ex.presidentCards.isEmpty
            else if ex.vpId.contains(playerId) then ex.vpCards.isEmpty
            else false
          val msg = ServerMessage.Exchange(
            role = role,
            target = target,
            count = count,
            isYourTurn = isYourTurn,
            hand = game.players(playerId).hand.map(_.toString)
          )
          queue.offer(msg).unit
        case None =>
          val player = game.players(playerId)
          val tempFinishOrder = game.finishOrder ++ game.activePlayers.map(_.id).toList
          val currentRanked = GameEngine.assignRoles(tempFinishOrder, game.players, game.autoTrouduc)
          val roleMap = currentRanked.map(p => p.id -> p.role).toMap
          val msg = ServerMessage.State(
            hand = player.hand.map(_.toString),
            table = game.lastPlay.map(p => s"${p.rank.courte} x${p.size}"),
            tableCards =
              game.lastPlay.map(_.cards.map(_.toString)).getOrElse(Nil),
            currentPlayer = game.currentPlayer.name,
            isYourTurn = game.currentPlayerIdx == playerId,
            players = game.players
              .map(p =>
                val roleStr = if !p.hasCards then roleMap.getOrElse(p.id, None).map(_.nom) else None
                PlayerInfo(p.name, p.cardCount, p.id == game.currentPlayerIdx, roleStr)
              )
              .toList,
            round = game.round
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
      entry.queue
        .offer(
          waitingMessage(entries, masterName, masterId, entry.id, isPlaying)
        )
        .unit
    }

  private def waitingMessage(
      entries: Vector[LobbyEntry],
      masterName: String,
      masterId: Option[Int],
      recipientId: Int,
      isPlaying: Boolean
  ): ServerMessage.Waiting =
    val needed = math.max(minPlayers - entries.size, 0)
    val isMaster = masterId.contains(recipientId)
    val canStart = !isPlaying && isMaster && entries.size >= minPlayers
    ServerMessage.Waiting(
      masterName,
      entries.map(_.name).toList,
      needed,
      isMaster,
      canStart,
      isPlaying
    )

  private def sendTo(
      lobby: Vector[LobbyEntry],
      id: Int,
      msg: ServerMessage
  ): UIO[Unit] =
    lobby.find(_.id == id).fold(ZIO.unit)(_.queue.offer(msg).unit)

object GameRoom:
  def make(minPlayers: Int): UIO[GameRoom] =
    Ref
      .make[RoomState](RoomState(Vector.empty, None, None, 0))
      .map(new GameRoom(_, minPlayers))
