package presifeur.server

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.json.*

class ProtocolSpec extends AnyFlatSpec with Matchers:

  // ── CardParser ────────────────────────────────────────────────────────────

  "CardParser" should "parser les symboles Unicode (♠♥♦♣)" in:
    CardParser.parse("3♠") shouldBe defined
    CardParser.parse("V♥") shouldBe defined
    CardParser.parse("10♦") shouldBe defined
    CardParser.parse("2♣") shouldBe defined

  it should "parser les codes lettre (P/C/K/T)" in:
    CardParser.parse("3P") shouldBe defined
    CardParser.parse("VC") shouldBe defined
    CardParser.parse("10K") shouldBe defined
    CardParser.parse("2T") shouldBe defined

  it should "parser toutes les valeurs de rang" in:
    val rangs = List("3","4","5","6","7","8","9","10","V","D","R","A","2")
    rangs.foreach(r => CardParser.parse(s"${r}P") shouldBe defined)

  it should "retourner None pour une entrée invalide" in:
    CardParser.parse("")    shouldBe empty
    CardParser.parse("X")   shouldBe empty
    CardParser.parse("5Z")  shouldBe empty
    CardParser.parse("ZP")  shouldBe empty
    CardParser.parse("1P")  shouldBe empty

  it should "produire la même carte via symbole ou code lettre" in:
    CardParser.parse("5♥") shouldBe CardParser.parse("5C")
    CardParser.parse("R♠") shouldBe CardParser.parse("RP")
    CardParser.parse("10♦") shouldBe CardParser.parse("10K")

  // ── ClientMessage (décodage JSON) ─────────────────────────────────────────

  "ClientMessage" should "décoder un message join" in:
    """{"tag":"join","name":"Alice"}""".fromJson[ClientMessage] shouldBe
      Right(ClientMessage.Join("Alice"))

  it should "décoder un message play avec plusieurs cartes" in:
    """{"tag":"play","cards":["5♥","5♠"]}""".fromJson[ClientMessage] shouldBe
      Right(ClientMessage.Play(List("5♥", "5♠")))

  it should "décoder un message play avec une seule carte" in:
    """{"tag":"play","cards":["R♣"]}""".fromJson[ClientMessage] shouldBe
      Right(ClientMessage.Play(List("R♣")))

  it should "décoder un message pass" in:
    """{"tag":"pass"}""".fromJson[ClientMessage] shouldBe Right(ClientMessage.Pass())

  it should "échouer sur un tag inconnu" in:
    """{"tag":"unknown"}""".fromJson[ClientMessage].isLeft shouldBe true

  it should "échouer si le champ name est absent d'un join" in:
    """{"tag":"join"}""".fromJson[ClientMessage].isLeft shouldBe true

  // ── ServerMessage (encodage JSON) ─────────────────────────────────────────

  "ServerMessage.Waiting" should "contenir le tag 'waiting' et le champ needed" in:
    val json = (ServerMessage.Waiting(List("Alice"), 2): ServerMessage).toJson
    json should include(""""tag":"waiting"""")
    json should include(""""needed":2""")
    json should include("Alice")

  "ServerMessage.State" should "contenir le tag 'state' et isYourTurn" in:
    val msg: ServerMessage = ServerMessage.State(
      hand          = List("3♠", "5♥"),
      table         = Some("5 x2"),
      currentPlayer = "Alice",
      isYourTurn    = true,
      players       = List(PlayerInfo("Alice", 17, true), PlayerInfo("Bob", 18, false)),
      round         = 1
    )
    val json = msg.toJson
    json should include(""""tag":"state"""")
    json should include(""""isYourTurn":true""")
    json should include(""""currentPlayer":"Alice"""")
    json should include(""""table":"5 x2"""")

  it should "omettre le champ table quand la table est vide" in:
    val msg: ServerMessage = ServerMessage.State(
      List("3♠"), None, "Alice", true, Nil, 1
    )
    msg.toJson should not include "\"table\""

  "ServerMessage.Error" should "contenir le tag 'error' et le message" in:
    val json = (ServerMessage.Error("Coup invalide"): ServerMessage).toJson
    json should include(""""tag":"error"""")
    json should include("Coup invalide")

  "ServerMessage.GameOver" should "contenir le tag 'gameOver' et les classements" in:
    val json = (ServerMessage.GameOver(
      List(RankingEntry("Président", "Alice"), RankingEntry("Trouduc", "Bob"))
    ): ServerMessage).toJson
    json should include(""""tag":"gameOver"""")
    json should include("Président")
    json should include("Trouduc")
