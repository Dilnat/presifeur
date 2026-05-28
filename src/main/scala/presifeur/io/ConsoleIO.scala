package presifeur.io

import presifeur.model.*

object ConsoleIO:

  def printState(state: GameState): Unit =
    println(s"\n=== Round ${state.round} | Turn: ${state.currentPlayer.name} ===")
    state.lastPlay match
      case None       => println("Table is clear — open with any play.")
      case Some(play) => println(s"Table: ${play.rank} x${play.size} (${play.cards.mkString(", ")})")
    println(s"Your hand: ${state.currentPlayer.hand.mkString(", ")}")
    state.players.foreach { p =>
      val roleStr = p.role.fold("")(r => s" [${r}]")
      println(s"  ${p.name}$roleStr: ${p.cardCount} cards")
    }

  def readPlay(hand: List[Card]): Option[List[Card]] =
    print("Cards to play (e.g. '3S 3H') or 'pass': ")
    val input = scala.io.StdIn.readLine().trim
    if input.equalsIgnoreCase("pass") then None
    else
      val tokens = input.split("\\s+").toList
      val parsed = tokens.flatMap(parseCard)
      if parsed.size != tokens.size then
        println("Could not parse some cards. Try again.")
        readPlay(hand)
      else Some(parsed)

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
