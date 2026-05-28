package presifeur.engine

import presifeur.model.*

object GameEngine:

  def newGame(playerNames: List[String]): GameState =
    require(playerNames.size >= 3, "Président requires at least 3 players")
    val deck   = Deck.shuffled()
    val hands  = Deck.deal(deck, playerNames.size)
    val players = playerNames.zipWithIndex.map { (name, i) =>
      Player(i, name, hands(i))
    }.toVector
    // Player holding 3♣ goes first
    val firstIdx = players.indexWhere(_.hand.exists(c => c.rank == Rank.Trois && c.suit == Suit.Trefles))
    GameState(players, firstIdx.max(0), None, 0, Nil, 1)

  def applyPlay(state: GameState, cards: List[Card]): Either[String, GameState] =
    Play(cards).flatMap { play =>
      val player = state.currentPlayer
      if !cards.forall(player.hand.contains) then
        Left("Player does not hold those cards")
      else state.lastPlay match
        case Some(last) if !play.beats(last) =>
          Left(s"Play does not beat the current table (${last.rank} x${last.size})")
        case Some(last) if play.size != last.size =>
          Left(s"Must play the same number of cards (${last.size})")
        case _ =>
          val updatedPlayer = player.removeCards(cards)
          val updatedPlayers = state.players.updated(state.currentPlayerIdx, updatedPlayer)
          val newFinishOrder =
            if updatedPlayer.hasCards then state.finishOrder
            else state.finishOrder :+ player.id
          Right(state.copy(
            players         = updatedPlayers,
            currentPlayerIdx= state.nextPlayerIdx,
            lastPlay        = Some(play),
            passCount       = 0,
            finishOrder     = newFinishOrder
          ))
    }

  def applyPass(state: GameState): Either[String, GameState] =
    val activeSinceLastPlay = state.activePlayers.size
    val newPassCount = state.passCount + 1
    // If everyone else passed, clear the table
    val clearTable = newPassCount >= activeSinceLastPlay - 1
    Right(state.copy(
      currentPlayerIdx = state.nextPlayerIdx,
      lastPlay         = if clearTable then None else state.lastPlay,
      passCount        = if clearTable then 0 else newPassCount
    ))

  def assignRoles(finishOrder: List[Int], players: Vector[Player]): Vector[Player] =
    val n = players.size
    val roles = n match
      case 3 => List(Role.President, Role.Neutre, Role.Trouduc)
      case 4 => List(Role.President, Role.VicePresident, Role.ViceTrouduc, Role.Trouduc)
      case _ => List(Role.President, Role.VicePresident) ++
                List.fill(n - 4)(Role.Neutre) ++
                List(Role.ViceTrouduc, Role.Trouduc)
    val roleMap = finishOrder.zip(roles).toMap
    players.map(p => p.copy(role = roleMap.get(p.id)))
