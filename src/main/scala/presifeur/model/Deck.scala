package presifeur.model

import scala.util.Random

object Deck:
  val full: List[Card] =
    for
      suit <- Suit.values.toList
      rank <- Rank.values.toList
    yield Card(rank, suit)

  def shuffled(seed: Option[Long] = None): List[Card] =
    val rng = seed.fold(Random())(Random(_))
    rng.shuffle(full)

  def deal(cards: List[Card], playerCount: Int): Vector[List[Card]] =
    val hands = Array.fill(playerCount)(List.empty[Card])
    cards.zipWithIndex.foreach { (card, i) =>
      hands(i % playerCount) = card :: hands(i % playerCount)
    }
    hands.map(_.sorted).toVector
