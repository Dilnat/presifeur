package presifeur.model

enum Suit:
  case Piques, Coeurs, Carreaux, Trefles

  def symbole: String = this match
    case Piques   => "♠"
    case Coeurs   => "♥"
    case Carreaux => "♦"
    case Trefles  => "♣"

enum Rank(val value: Int):
  case Trois  extends Rank(3)
  case Quatre extends Rank(4)
  case Cinq   extends Rank(5)
  case Six    extends Rank(6)
  case Sept   extends Rank(7)
  case Huit   extends Rank(8)
  case Neuf   extends Rank(9)
  case Dix    extends Rank(10)
  case Valet  extends Rank(11)
  case Dame   extends Rank(12)
  case Roi    extends Rank(13)
  case As     extends Rank(14)
  case Deux   extends Rank(15) // le Deux est la carte la plus haute au président

  def courte: String = this match
    case Valet => "V"
    case Dame  => "D"
    case Roi   => "R"
    case As    => "A"
    case Deux  => "2"
    case other => other.value.toString

case class Card(rank: Rank, suit: Suit):
  override def toString: String = s"${rank.courte}${suit.symbole}"

given Ordering[Card] = Ordering.by(_.rank.value)
