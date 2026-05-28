package presifeur.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import presifeur.model.*

class GameEngineSpec extends AnyFlatSpec with Matchers:

  val players = List("Alice", "Bob", "Carol")

  "GameEngine.newGame" should "deal all 52 cards" in:
    val state = GameEngine.newGame(players)
    state.players.map(_.cardCount).sum shouldBe 52

  it should "require at least 3 players" in:
    an[IllegalArgumentException] should be thrownBy GameEngine.newGame(List("A", "B"))

  "GameEngine.applyPlay" should "reject cards the player doesn't hold" in:
    val state = GameEngine.newGame(players)
    val fakeCard = Card(Rank.Ace, Suit.Spades)
    val currentHand = state.currentPlayer.hand
    val notInHand = List(fakeCard).filterNot(currentHand.contains)
    if notInHand.nonEmpty then
      GameEngine.applyPlay(state, notInHand).isLeft shouldBe true

  "GameEngine.applyPass" should "clear the table when all others have passed" in:
    val state = GameEngine.newGame(players)
    // Simulate two consecutive passes (3 players, so 2 passes clears)
    val s1 = GameEngine.applyPass(state).toOption.get
    val s2 = GameEngine.applyPass(s1).toOption.get
    s2.lastPlay shouldBe None

  "Play" should "beat a lower play of the same size" in:
    val low  = Play(List(Card(Rank.Five, Suit.Hearts))).toOption.get
    val high = Play(List(Card(Rank.King, Suit.Spades))).toOption.get
    high.beats(low) shouldBe true
    low.beats(high) shouldBe false

  it should "not beat a play of different size" in:
    val single = Play(List(Card(Rank.King, Suit.Spades))).toOption.get
    val pair   = Play(List(Card(Rank.Ace, Suit.Hearts), Card(Rank.Ace, Suit.Clubs))).toOption.get
    pair.beats(single) shouldBe false
