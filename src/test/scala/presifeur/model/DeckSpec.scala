package presifeur.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeckSpec extends AnyFlatSpec with Matchers:

  "Deck.full" should "contain 52 cards" in:
    Deck.full.size shouldBe 52

  it should "contain all suits and ranks" in:
    Deck.full.map(_.suit).distinct.size shouldBe 4
    Deck.full.map(_.rank).distinct.size shouldBe 13

  "Deck.deal" should "distribute all cards" in:
    val hands = Deck.deal(Deck.full, 3)
    hands.map(_.size).sum shouldBe 52

  it should "give each player roughly equal cards" in:
    val hands = Deck.deal(Deck.full, 3)
    hands.foreach(h => h.size should (be >= 17 and be <= 18))
