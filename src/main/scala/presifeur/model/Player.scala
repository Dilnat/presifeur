package presifeur.model

enum Role:
  case President, VicePresident, Neutral, ViceAsshole, Asshole

case class Player(id: Int, name: String, hand: List[Card], role: Option[Role] = None):
  def hasCards: Boolean    = hand.nonEmpty
  def cardCount: Int       = hand.size
  def removeCards(cards: List[Card]): Player =
    copy(hand = hand.filterNot(cards.contains))
