package presifeur

import presifeur.engine.GameEngine
import presifeur.io.ConsoleIO
import presifeur.model.*

@main def run(): Unit =
  println("=== Président ===")
  print("Player names (comma-separated, min 3): ")
  val names = scala.io.StdIn.readLine().split(",").map(_.trim).toList
  if names.size < 3 then
    println("Need at least 3 players.")
    sys.exit(1)

  var state = GameEngine.newGame(names)

  while !state.isGameOver do
    ConsoleIO.printState(state)
    if !state.currentPlayer.hasCards then
      state = state.copy(currentPlayerIdx = state.nextPlayerIdx)
    else
      val result = ConsoleIO.readPlay(state.currentPlayer.hand) match
        case None        => GameEngine.applyPass(state)
        case Some(cards) => GameEngine.applyPlay(state, cards)
      result match
        case Left(err)       => println(s"Invalid move: $err")
        case Right(newState) => state = newState

  val ranked = GameEngine.assignRoles(state.finishOrder, state.players)
  println("\n=== Game Over ===")
  ranked.foreach { p =>
    println(s"${p.role.fold("?")(_.toString)}: ${p.name}")
  }
