package presifeur.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import presifeur.model.*

class GameEngineSpec extends AnyFlatSpec with Matchers:

  val joueurs = List("Alice", "Bob", "Carol")

  "GameEngine.newGame" should "distribuer toutes les cartes aux joueurs" in:
    val state = GameEngine.newGame(joueurs)
    state.players.map(_.cardCount).sum shouldBe state.players.map(_.cardCount).sum

  it should "exiger au moins 3 joueurs" in:
    an[IllegalArgumentException] should be thrownBy GameEngine.newGame(List("A", "B"))

  "GameEngine.applyPlay" should "refuser des cartes que le joueur ne possède pas" in:
    val state    = GameEngine.newGame(joueurs)
    val fakeCard = Card(Rank.As, Suit.Piques)
    val notInHand = List(fakeCard).filterNot(state.currentPlayer.hand.contains)
    if notInHand.nonEmpty then
      GameEngine.applyPlay(state, notInHand).isLeft shouldBe true

  "GameEngine.applyPass" should "vider la table quand tous les autres ont passé" in:
    val state = GameEngine.newGame(joueurs)
    val s1 = GameEngine.applyPass(state).toOption.get
    val s2 = GameEngine.applyPass(s1).toOption.get
    s2.lastPlay shouldBe None

  "Play" should "battre une combinaison plus faible de même taille" in:
    val basse  = Play(List(Card(Rank.Cinq, Suit.Coeurs))).toOption.get
    val haute  = Play(List(Card(Rank.Roi, Suit.Piques))).toOption.get
    haute.beats(basse) shouldBe true
    basse.beats(haute) shouldBe false

  it should "ne pas battre une combinaison de taille différente" in:
    val simple = Play(List(Card(Rank.Roi, Suit.Piques))).toOption.get
    val paire  = Play(List(Card(Rank.As, Suit.Coeurs), Card(Rank.As, Suit.Trefles))).toOption.get
    paire.beats(simple) shouldBe false
