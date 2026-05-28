package presifeur.model

enum Role:
  case President, VicePresident, Neutre, ViceTrouduc, Trouduc

  def nom: String = this match
    case President     => "Président"
    case VicePresident => "Vice-Président"
    case Neutre        => "Neutre"
    case ViceTrouduc   => "Vice-Trouduc"
    case Trouduc       => "Trouduc"

case class Player(id: Int, name: String, hand: List[Card], role: Option[Role] = None):
  def hasCards: Boolean = hand.nonEmpty
  def cardCount: Int    = hand.size
  def removeCards(cards: List[Card]): Player =
    copy(hand = hand.filterNot(cards.contains))
