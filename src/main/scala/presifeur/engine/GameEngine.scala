package presifeur.engine

import presifeur.model.*

object GameEngine:

  def newGame(playerNames: List[String]): GameState =
    require(playerNames.size >= 3, "Président requires at least 3 players")
    val deck  = Deck.shuffled()
    val hands = Deck.deal(deck, playerNames.size)
    val players = playerNames.zipWithIndex.map { (name, i) =>
      Player(i, name, hands(i))
    }.toVector
    val firstIdx = players.indexWhere(_.hand.exists(c => c.rank == Rank.Trois && c.suit == Suit.Trefles))
    GameState(players, firstIdx.max(0), None, 0, Nil, 1)

  def applyPlay(state: GameState, cards: List[Card]): Either[String, GameState] =
    Play(cards).flatMap { play =>
      val player = state.currentPlayer
      if !cards.forall(player.hand.contains) then
        Left("Le joueur ne possède pas ces cartes")
      else state.lastPlay match
        case Some(last) if play.size != last.size =>
          Left(s"Il faut jouer ${last.size} carte(s)")
        case Some(last) if state.sameRankStreak >= 2 && play.rank != last.rank =>
          Left(s"Forcé : vous devez jouer ${last.rank.courte} ou passer")
        case Some(last) if play.rank.value < last.rank.value =>
          Left(s"Le coup ne bat pas la table (${last.rank.courte} x${last.size})")
        case _ =>
          val updatedPlayer  = player.removeCards(cards)
          val playerFinished = !updatedPlayer.hasCards
          val endsWithTwo    = playerFinished && play.rank == Rank.Deux

          val updatedPlayers = state.players.updated(state.currentPlayerIdx, updatedPlayer)
          val newFinishOrder =
            if !playerFinished || endsWithTwo then state.finishOrder
            else state.finishOrder :+ player.id
          val newAutoTrouduc =
            if endsWithTwo then state.autoTrouduc :+ player.id else state.autoTrouduc

          // Cumul de cartes du rang actuel jouées dans cette manche
          val newRankCount = state.lastPlay match
            case Some(last) if play.rank == last.rank => state.currentRankCount + play.size
            case _                                    => play.size
          // 4 cartes du même rang = fermeture de la manche
          val closedByFour = newRankCount >= 4

          // Le 1er joueur à finir (Président) vide la table
          // Les suivants gardent leur dernière carte sur la table
          val isFirstToFinish = playerFinished && !endsWithTwo &&
                                state.finishOrder.isEmpty && state.autoTrouduc.isEmpty
          val tableCleared = play.rank == Rank.Deux || closedByFour || isFirstToFinish

          val newSameRankStreak =
            if tableCleared then 0
            else state.lastPlay match
              case Some(last) if play.rank == last.rank => state.sameRankStreak + 1
              case _                                    => 1

          // Le joueur qui ferme avec un 2 ou un carré recommence ; sinon c'est le suivant
          val startsNextManche = (play.rank == Rank.Deux || closedByFour) && !playerFinished
          val nextIdx = if startsNextManche then state.currentPlayerIdx else state.nextPlayerIdx

          Right(state.copy(
            players          = updatedPlayers,
            currentPlayerIdx = nextIdx,
            lastPlay         = if tableCleared then None else Some(play),
            passCount        = 0,
            finishOrder      = newFinishOrder,
            round            = if tableCleared then state.round + 1 else state.round,
            sameRankStreak   = newSameRankStreak,
            autoTrouduc      = newAutoTrouduc,
            currentRankCount = if tableCleared then 0 else newRankCount
          ))
    }

  def applyPass(state: GameState): Either[String, GameState] =
    val newPassCount     = state.passCount + 1
    // La passe forcée réinitialise le streak mais ne vide pas la table
    // Le joueur suivant joue normalement (doit battre la table)
    val forcedStreakReset = state.sameRankStreak >= 2
    val normalClear      = newPassCount >= state.activePlayers.size - 1
    val tableClear       = normalClear && state.lastPlay.isDefined
    val streakReset      = forcedStreakReset || tableClear
    Right(state.copy(
      currentPlayerIdx = state.nextPlayerIdx,
      lastPlay         = if tableClear then None else state.lastPlay,
      passCount        = if tableClear then 0 else newPassCount,
      round            = if tableClear then state.round + 1 else state.round,
      sameRankStreak   = if streakReset then 0 else state.sameRankStreak,
      currentRankCount = if streakReset then 0 else state.currentRankCount
    ))

  def canPlay(hand: List[Card], state: GameState): Boolean =
    state.lastPlay match
      case None       => true
      case Some(last) =>
        val n = last.size
        if state.sameRankStreak >= 2 then
          hand.count(_.rank == last.rank) >= n
        else
          Rank.values.exists(r => r.value >= last.rank.value && hand.count(_.rank == r) >= n)

  def assignRoles(finishOrder: List[Int], players: Vector[Player], autoTrouduc: List[Int] = Nil): Vector[Player] =
    val n              = players.size
    val normalOrder    = finishOrder.filterNot(autoTrouduc.contains)
    val effectiveOrder = normalOrder ++ autoTrouduc
    // Les joueurs qui finissent sur un 2 sont Trouduc automatique peu importe leur rang
    val roleMap: Map[Int, Role] = n match
      case 3 =>
        // Le joueur du milieu n'a pas de rôle
        List(
          effectiveOrder.lift(0).map(_ -> Role.President),
          effectiveOrder.lift(2).map(_ -> Role.Trouduc)
        ).flatten.toMap
      case 4 =>
        effectiveOrder.zip(List(Role.President, Role.VicePresident, Role.ViceTrouduc, Role.Trouduc)).toMap
      case _ =>
        val roles = List(Role.President, Role.VicePresident) ++
                    List.fill(n - 4)(Role.Neutre) ++
                    List(Role.ViceTrouduc, Role.Trouduc)
        effectiveOrder.zip(roles).toMap
    players.map(p => p.copy(role = roleMap.get(p.id)))
