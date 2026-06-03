package presifeur.server

import presifeur.model.*
import zio.json.*

// ── Client → Serveur ─────────────────────────────────────────────────────────

@jsonDiscriminator("tag")
sealed trait ClientMessage
object ClientMessage:
  @jsonHint("join")  case class Join(name: String)        extends ClientMessage
  @jsonHint("start") case class Start()                   extends ClientMessage
  @jsonHint("play")  case class Play(cards: List[String]) extends ClientMessage
  @jsonHint("pass")  case class Pass()                    extends ClientMessage

  given JsonDecoder[ClientMessage] = DeriveJsonDecoder.gen[ClientMessage]

// ── Serveur → Client ─────────────────────────────────────────────────────────

case class PlayerInfo(name: String, cardCount: Int, isCurrentPlayer: Boolean) derives JsonEncoder
case class RankingEntry(role: String, name: String) derives JsonEncoder

@jsonDiscriminator("tag")
sealed trait ServerMessage
object ServerMessage:
  @jsonHint("waiting")
  case class Waiting(
    master: String,
    players: List[String],
    needed: Int,
    isMaster: Boolean,
    canStart: Boolean,
    isPlaying: Boolean
  ) extends ServerMessage

  @jsonHint("state")
  case class State(
    hand: List[String],
    table: Option[String],
    tableCards: List[String],
    currentPlayer: String,
    isYourTurn: Boolean,
    players: List[PlayerInfo],
    round: Int
  ) extends ServerMessage

  @jsonHint("error")
  case class Error(message: String) extends ServerMessage

  @jsonHint("gameOver")
  case class GameOver(rankings: List[RankingEntry]) extends ServerMessage

  given JsonEncoder[ServerMessage] = DeriveJsonEncoder.gen[ServerMessage]

// ── Parseur de cartes partagé CLI / serveur ───────────────────────────────────

object CardParser:
  def parse(s: String): Option[Card] =
    if s.length < 2 then None
    else
      val suit = s.last.toString match
        case "♠" | "P" => Some(Suit.Piques)
        case "♥" | "C" => Some(Suit.Coeurs)
        case "♦" | "K" => Some(Suit.Carreaux)
        case "♣" | "T" => Some(Suit.Trefles)
        case _          => None
      val rank = s.dropRight(1).toUpperCase match
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
      for r <- rank; su <- suit yield Card(r, su)
