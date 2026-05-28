package presifeur.io

import presifeur.model.*
import zio.*

object ConsoleIO:

  def printState(state: GameState): UIO[Unit] =
    val tableInfo = state.lastPlay match
      case None       => "Table vide — ouvrez avec n'importe quelle combinaison."
      case Some(play) => s"Table : ${play.rank.courte} x${play.size} (${play.cards.mkString(", ")})"
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

  def readPlay(hand: List[Card]): Task[Option[List[Card]]] =
    Console.print("Cartes à jouer (ex: '3♠ R♥') ou 'passer' : ").orDie *>
    Console.readLine.flatMap { input =>
      val trimmed = input.trim
      if trimmed.equalsIgnoreCase("passer") then ZIO.succeed(None)
      else
        val tokens = trimmed.split("\\s+").toList
        val parsed = tokens.flatMap(parseCard)
        if parsed.size != tokens.size then
          Console.printLine("Cartes non reconnues. Réessayez.").orDie *> readPlay(hand)
        else ZIO.succeed(Some(parsed))
    }

  // Codes de saisie : rang (3-9, 10, V, D, R, A, 2) + couleur (P=Piques, C=Coeurs, K=Carreaux, T=Trefles)
  // Exemple : VP = Valet de Piques, DC = Dame de Coeurs, 10K = Dix de Carreaux
  private def parseCard(s: String): Option[Card] =
    if s.length < 2 then None
    else
      val suitChar = s.last.toUpper
      val rankStr  = s.dropRight(1).toUpperCase
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
      val suit = suitChar match
        case 'P' => Some(Suit.Piques)
        case 'C' => Some(Suit.Coeurs)
        case 'K' => Some(Suit.Carreaux)
        case 'T' => Some(Suit.Trefles)
        case _   => None
      for r <- rank; s <- suit yield Card(r, s)
