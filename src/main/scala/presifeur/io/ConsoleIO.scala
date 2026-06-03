package presifeur.io

import presifeur.model.*
import zio.*

object ConsoleIO:

  def printState(state: GameState): UIO[Unit] =
    val tableInfo = state.lastPlay match
      case None       => "Table vide — ouvrez avec n'importe quelle combinaison."
      case Some(play) if state.sameRankStreak >= 2 =>
        s"Table : ${play.rank.courte} x${play.size} (${play.cards.mkString(", ")}) — FORCÉ : jouez ${play.rank.courte} ou passez"
      case Some(play) =>
        s"Table : ${play.rank.courte} x${play.size} (${play.cards.mkString(", ")})"
    val playerList = state.players.map { p =>
      val roleStr = p.role.fold("")(r => s" [${r.nom}]")
      s"  ${p.name}$roleStr : ${p.cardCount} carte(s)"
    }.mkString("\n")
    Console.printLine(
      s"""
         |=== Manche ${state.round} | Tour de : ${state.currentPlayer.name} ===
         |$tableInfo
         |Votre main : ${state.currentPlayer.hand.mkString(", ")}
         |$playerList""".stripMargin
    ).orDie

  def readPlay(hand: List[Card], forcedRank: Option[Rank] = None, mustPass: Boolean = false): Task[Option[List[Card]]] =
    val prompt =
      if mustPass then "Tapez 'passer' : "
      else forcedRank.fold(
        "Cartes à jouer (ex: '5♥ 5♠' ou '5C 5P') ou 'passer' : "
      )(r => s"Forcé — jouez des ${r.courte} (ex: '${r.courte}♥') ou 'passer' : ")
    Console.print(prompt).orDie *>
    Console.readLine.flatMap { input =>
      val trimmed = input.trim
      if trimmed.equalsIgnoreCase("passer") then ZIO.succeed(None)
      else if mustPass then
        Console.printLine("Vous devez taper 'passer'.").orDie *> readPlay(hand, forcedRank, mustPass)
      else
        val tokens = trimmed.split("\\s+").toList
        val parsed = tokens.flatMap(parseCard)
        if parsed.size != tokens.size then
          Console.printLine("Cartes non reconnues. Réessayez.").orDie *> readPlay(hand, forcedRank, mustPass)
        else ZIO.succeed(Some(parsed))
    }

  // Échange de cartes entre deux parties
  def applyExchange(baseState: GameState, prevRanked: Vector[Player]): Task[GameState] =
    val roleByName      = prevRanked.flatMap(p => p.role.map(p.name -> _)).toMap
    val playersWithRoles = baseState.players.map(p => p.copy(role = roleByName.get(p.name)))
    val state           = baseState.copy(players = playersWithRoles)

    def idOf(role: Role): Option[Int] =
      state.players.find(_.role.contains(role)).map(_.id)

    val presId = idOf(Role.President)
    val vpId   = idOf(Role.VicePresident)
    val vtId   = idOf(Role.ViceTrouduc)
    val trdId  = idOf(Role.Trouduc)

    for
      _  <- Console.printLine("\n=== Échange de cartes ===").orDie
      // Les perdants donnent leurs meilleures cartes en premier
      s1 <- (trdId, presId) match
              case (Some(f), Some(t)) => giveAutoBest(state, f, t, 2)
              case _                  => ZIO.succeed(state)
      s2 <- (vtId, vpId) match
              case (Some(f), Some(t)) => giveAutoBest(s1, f, t, 1)
              case _                  => ZIO.succeed(s1)
      // Les gagnants choisissent ce qu'ils redonnent
      s3 <- (presId, trdId) match
              case (Some(f), Some(t)) => giveChosen(s2, f, t, 2)
              case _                  => ZIO.succeed(s2)
      s4 <- (vpId, vtId) match
              case (Some(f), Some(t)) => giveChosen(s3, f, t, 1)
              case _                  => ZIO.succeed(s3)
    yield s4

  private def giveAutoBest(state: GameState, fromIdx: Int, toIdx: Int, count: Int): Task[GameState] =
    val from      = state.players(fromIdx)
    val to        = state.players(toIdx)
    val bestCards = from.hand.sortBy(_.rank.value).reverse.take(count)
    val newFrom   = from.copy(hand = from.hand.filterNot(bestCards.contains))
    val newTo     = to.copy(hand = (to.hand ++ bestCards).sortBy(_.rank.value))
    Console.printLine(s"${from.name} (${from.role.fold("")(_.nom)}) — main : ${from.hand.mkString(", ")}").orDie *>
    Console.printLine(s"  → donne ses $count meilleure(s) carte(s) : ${bestCards.mkString(", ")} à ${to.name} (${to.role.fold("")(_.nom)})").orDie *>
    ZIO.succeed(state.copy(players = state.players.updated(fromIdx, newFrom).updated(toIdx, newTo)))

  private def giveChosen(state: GameState, fromIdx: Int, toIdx: Int, count: Int): Task[GameState] =
    val from     = state.players(fromIdx)
    val to       = state.players(toIdx)
    val fromRole = from.role.fold("?")(_.nom)
    val toRole   = to.role.fold("?")(_.nom)
    for
      _      <- Console.printLine(s"\n${from.name} ($fromRole), donnez $count carte(s) à ${to.name} ($toRole)").orDie
      _      <- Console.printLine(s"Votre main : ${from.hand.mkString(", ")}").orDie
      _      <- Console.print(s"Cartes à donner : ").orDie
      chosen <- readGiveCards(from.hand, count)
      newFrom = from.copy(hand = from.hand.filterNot(chosen.contains))
      newTo   = to.copy(hand = (to.hand ++ chosen).sortBy(_.rank.value))
      _      <- Console.printLine(s"${from.name} donne ${chosen.mkString(", ")} à ${to.name}").orDie
    yield state.copy(players = state.players.updated(fromIdx, newFrom).updated(toIdx, newTo))

  private def readGiveCards(hand: List[Card], count: Int): Task[List[Card]] =
    Console.readLine.flatMap { input =>
      val tokens = input.trim.split("\\s+").toList
      val parsed = tokens.flatMap(parseCard)
      if parsed.size != tokens.size then
        Console.printLine("Cartes non reconnues. Réessayez.").orDie *>
        Console.print("Cartes à donner : ").orDie *>
        readGiveCards(hand, count)
      else if parsed.size != count then
        Console.printLine(s"Il faut exactement $count carte(s). Réessayez.").orDie *>
        Console.print("Cartes à donner : ").orDie *>
        readGiveCards(hand, count)
      else if !parsed.forall(hand.contains) then
        Console.printLine("Vous ne possédez pas ces cartes. Réessayez.").orDie *>
        Console.print("Cartes à donner : ").orDie *>
        readGiveCards(hand, count)
      else
        ZIO.succeed(parsed)
    }

  // Accepte rang + couleur, la couleur pouvant être un symbole (♠♥♦♣) ou une lettre (P/C/K/T)
  // Exemples : 5♥  VP  DC  10♦  RT
  private def parseCard(s: String): Option[Card] =
    if s.length < 2 then None
    else
      val suitStr = s.last.toString
      val rankStr = s.dropRight(1).toUpperCase
      val rank = rankStr match
        case "3"  => Some(Rank.Trois)
        case "4"  => Some(Rank.Quatre)
        case "5"  => Some(Rank.Cinq)
        case "6"  => Some(Rank.Six)
        case "7"  => Some(Rank.Sept)
        case "8"  => Some(Rank.Huit)
        case "9"  => Some(Rank.Neuf)
        case "10" => Some(Rank.Dix)
        case "V"  => Some(Rank.Valet)
        case "D"  => Some(Rank.Dame)
        case "R"  => Some(Rank.Roi)
        case "A"  => Some(Rank.As)
        case "2"  => Some(Rank.Deux)
        case _    => None
      val suit = suitStr match
        case "♠" | "P" => Some(Suit.Piques)
        case "♥" | "C" => Some(Suit.Coeurs)
        case "♦" | "K" => Some(Suit.Carreaux)
        case "♣" | "T" => Some(Suit.Trefles)
        case _          => None
      for r <- rank; s <- suit yield Card(r, s)
