package presifeur.model

/** A play is a set of cards of the same rank played at once. */
opaque type Play = List[Card]

object Play:
  def apply(cards: List[Card]): Either[String, Play] =
    if cards.isEmpty then Left("A play must contain at least one card")
    else if cards.map(_.rank).distinct.size > 1 then Left("All cards in a play must have the same rank")
    else Right(cards)

  extension (p: Play)
    def cards: List[Card]     = p
    def rank: Rank            = p.head.rank
    def size: Int             = p.size
    def beats(other: Play): Boolean =
      p.size == other.size && p.rank.value > other.rank.value
