package presifeur.model

enum Suit:
  case Spades, Hearts, Diamonds, Clubs

enum Rank(val value: Int):
  case Three  extends Rank(3)
  case Four   extends Rank(4)
  case Five   extends Rank(5)
  case Six    extends Rank(6)
  case Seven  extends Rank(7)
  case Eight  extends Rank(8)
  case Nine   extends Rank(9)
  case Ten    extends Rank(10)
  case Jack   extends Rank(11)
  case Queen  extends Rank(12)
  case King   extends Rank(13)
  case Ace    extends Rank(14)
  case Two    extends Rank(15) // Two is the highest card in président

case class Card(rank: Rank, suit: Suit):
  override def toString: String = s"${rank.toString.head}${suit.toString.head}"

given Ordering[Card] = Ordering.by(_.rank.value)
