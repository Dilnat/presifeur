package presifeur.server

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.*

class GameRoomSpec extends AnyFlatSpec with Matchers:

  // ── Helper d'exécution ZIO en contexte de test ────────────────────────────

  private def run[A](zio: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit u =>
      Runtime.default.unsafe.run(zio).getOrThrowFiberFailure()
    }

  private def mkQueue(): Queue[ServerMessage] = run(Queue.unbounded[ServerMessage])
  private def mkRoom(min: Int = 3): GameRoom  = run(GameRoom.make(min))

  /** Vide la queue et retourne tous les messages disponibles. */
  private def drain(q: Queue[ServerMessage]): List[ServerMessage] =
    run(q.takeAll).toList

  /** Lance une partie avec n joueurs fictifs et retourne (room, queues, états initiaux). */
  private def startGame(n: Int = 3): (GameRoom, Vector[Queue[ServerMessage]], Vector[ServerMessage.State]) =
    val room   = mkRoom(n)
    val queues = Vector.fill(n)(mkQueue())
    queues.zipWithIndex.foreach { (q, i) =>
      run(room.join(s"Joueur$i", q).either)
    }
    // Récupérer l'état initial de chaque joueur (dernier message reçu)
    val states = queues.map { q =>
      drain(q).collect { case s: ServerMessage.State => s }.last
    }
    (room, queues, states)

  // ── Lobby ─────────────────────────────────────────────────────────────────

  "GameRoom.join" should "assigner les IDs dans l'ordre de connexion" in:
    val room = mkRoom()
    val ids = (0 until 3).map { _ =>
      run(room.join(s"J", mkQueue()).either).toOption.get
    }
    ids shouldBe Vector(0, 1, 2)

  it should "envoyer 'waiting' aux joueurs en attente" in:
    val room = mkRoom()
    val q0 = mkQueue()
    run(room.join("Alice", q0).either)
    val msgs = drain(q0)
    msgs should have size 1
    msgs.head shouldBe a[ServerMessage.Waiting]

  it should "mettre à jour le compteur 'needed' à chaque connexion" in:
    val room = mkRoom()
    val q0 = mkQueue(); val q1 = mkQueue()
    run(room.join("Alice", q0).either)
    drain(q0)  // vider
    run(room.join("Bob", q1).either)
    val waiting = drain(q0).head.asInstanceOf[ServerMessage.Waiting]
    waiting.needed shouldBe 1
    waiting.players should contain allOf ("Alice", "Bob")

  it should "démarrer la partie et envoyer 'state' quand assez de joueurs" in:
    val (_, queues, states) = startGame(3)
    states should have size 3
    states.foreach(_ shouldBe a[ServerMessage.State])
    // Exactement un joueur a isYourTurn = true
    states.count(_.isYourTurn) shouldBe 1

  it should "refuser une connexion supplémentaire une fois la partie lancée" in:
    val (room, _, _) = startGame(3)
    val result = run(room.join("Intrus", mkQueue()).either)
    result.isLeft shouldBe true

  it should "distribuer 52 cartes au total entre les joueurs" in:
    val (_, _, states) = startGame(3)
    states.map(_.hand.size).sum shouldBe 52

  // ── Tour de jeu ───────────────────────────────────────────────────────────

  "GameRoom.play" should "refuser si ce n'est pas le tour du joueur" in:
    val (room, queues, states) = startGame(3)
    val notCurrentId = states.indexWhere(!_.isYourTurn)
    run(room.play(notCurrentId, List("3P")))
    val msgs = drain(queues(notCurrentId))
    msgs.head shouldBe a[ServerMessage.Error]

  it should "refuser des cartes que le joueur ne possède pas" in:
    val (room, queues, states) = startGame(3)
    val currentId = states.indexWhere(_.isYourTurn)
    // Jouer une carte qui n'est certainement pas dans la main (main vide fictive)
    run(room.play(currentId, List("ZZZZ")))
    val msgs = drain(queues(currentId))
    msgs.head shouldBe a[ServerMessage.Error]

  it should "accepter un coup valide et diffuser le nouvel état à tous" in:
    val (room, queues, states) = startGame(3)
    val currentId  = states.indexWhere(_.isYourTurn)
    val firstCard  = states(currentId).hand.head
    run(room.play(currentId, List(firstCard)))
    // Tous les joueurs reçoivent un message (state ou autre)
    queues.foreach { q => drain(q) should not be empty }

  it should "mettre à jour la main du joueur après un coup valide" in:
    val (room, queues, states) = startGame(3)
    val currentId = states.indexWhere(_.isYourTurn)
    val handBefore = states(currentId).hand
    val firstCard  = handBefore.head
    run(room.play(currentId, List(firstCard)))
    val newState = drain(queues(currentId))
      .collect { case s: ServerMessage.State => s }
      .head
    newState.hand should have size (handBefore.size - 1)
    newState.hand should not contain firstCard

  // ── Passer ────────────────────────────────────────────────────────────────

  "GameRoom.pass" should "refuser si ce n'est pas le tour du joueur" in:
    val (room, queues, states) = startGame(3)
    val notCurrentId = states.indexWhere(!_.isYourTurn)
    run(room.pass(notCurrentId))
    drain(queues(notCurrentId)).head shouldBe a[ServerMessage.Error]

  it should "avancer le tour au joueur suivant" in:
    val (room, queues, states) = startGame(3)
    val currentId = states.indexWhere(_.isYourTurn)
    run(room.pass(currentId))
    val newStates = queues.map { q =>
      drain(q).collect { case s: ServerMessage.State => s }.headOption
    }
    // Le joueur courant a changé
    val newCurrentId = newStates.indexWhere(_.exists(_.isYourTurn))
    newCurrentId should not be currentId

  it should "vider la table quand tous les autres joueurs ont passé (3 joueurs = 2 passes)" in:
    val (room, queues, states) = startGame(3)
    val p0 = states.indexWhere(_.isYourTurn)
    val p1 = (p0 + 1) % 3
    val p2 = (p0 + 2) % 3

    // p0 joue une carte pour mettre quelque chose sur la table
    run(room.play(p0, List(states(p0).hand.head)))
    queues.foreach(drain)  // vider

    // p1 et p2 passent → table doit être vidée
    run(room.pass(p1))
    queues.foreach(drain)
    run(room.pass(p2))

    val statesAfter = queues.map(q =>
      drain(q).collect { case s: ServerMessage.State => s }.headOption
    )
    // La table doit être None (vidée)
    statesAfter.flatten.foreach(_.table shouldBe None)
