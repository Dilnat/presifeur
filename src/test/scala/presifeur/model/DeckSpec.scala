package presifeur.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeckSpec extends AnyFlatSpec with Matchers:

  "Deck.full" should "contenir 52 cartes" in:
    Deck.full.size shouldBe 52

  it should "couvrir toutes les couleurs et tous les rangs" in:
    Deck.full.map(_.suit).distinct.size shouldBe 4
    Deck.full.map(_.rank).distinct.size shouldBe 13

  "Deck.deal" should "distribuer toutes les cartes" in:
    val hands = Deck.deal(Deck.full, 3)
    hands.map(_.size).sum shouldBe 52

  it should "donner un nombre équitable de cartes à chaque joueur" in:
    val hands = Deck.deal(Deck.full, 3)
    hands.foreach(h => h.size should (be >= 17 and be <= 18))
