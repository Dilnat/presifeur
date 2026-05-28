package presifeur.io

import presifeur.model.*
import zio.*

object ConsoleIO:

  def printState(state: GameState): UIO[Unit] =
    val tableInfo = state.lastPlay match
      case None       => "Table is clear — open with any play."
      case Some(play) => s"Table: ${play.rank} x${play.size} (${play.cards.mkString(", ")})"
    val playerList = state.players.map { p =>
      val roleStr = p.role.fold("")(r => s" [$r]")
      s"  ${p.name}$roleStr: ${p.cardCount} cards"
    }.mkString("\n")
    Console.printLine(
      s"""
         |=== Round ${state.round} | Turn: ${state.currentPlayer.name} ===
         |$tableInfo
         |Your hand: ${state.currentPlayer.hand.mkString(", ")}
         |$playerList""".stripMargin
    ).orDie

  def readPlay(hand: List[Card]): Task[Option[List[Card]]] =
    Console.print("Cards to play (e.g. '3S 3H') or 'pass': ").orDie *>
    Console.readLine.flatMap { input =>
      val trimmed = input.trim
      if trimmed.equalsIgnoreCase("pass") then ZIO.succeed(None)
      else
        val tokens = trimmed.split("\\s+").toList
        val parsed = tokens.flatMap(parseCard)
        if parsed.size != tokens.size then
          Console.printLine("Could not parse some cards. Try again.").orDie *> readPlay(hand)
        else ZIO.succeed(Some(parsed))
    }

  private def parseCard(s: String): Option[Card] =
    if s.length < 2 then None
    else
      val rankChar = s.dropRight(1).toUpperCase
      val suitChar = s.last.toUpper
      val rank = rankChar match
        case "3"  => Some(Rank.Three)
        case "4"  => Some(Rank.Four)
        case "5"  => Some(Rank.Five)
        case "6"  => Some(Rank.Six)
        case "7"  => Some(Rank.Seven)
        case "8"  => Some(Rank.Eight)
        case "9"  => Some(Rank.Nine)
        case "10" => Some(Rank.Ten)
        case "J"  => Some(Rank.Jack)
        case "Q"  => Some(Rank.Queen)
        case "K"  => Some(Rank.King)
        case "A"  => Some(Rank.Ace)
        case "2"  => Some(Rank.Two)
        case _    => None
      val suit = suitChar match
        case 'S' => Some(Suit.Spades)
        case 'H' => Some(Suit.Hearts)
        case 'D' => Some(Suit.Diamonds)
        case 'C' => Some(Suit.Clubs)
        case _   => None
      for r <- rank; s <- suit yield Card(r, s)
