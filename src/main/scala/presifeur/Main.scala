package presifeur

import presifeur.engine.GameEngine
import presifeur.io.ConsoleIO
import presifeur.model.*
import zio.*

object Main extends ZIOAppDefault:

  override def run: Task[Unit] =
    for
      _     <- Console.printLine("=== Président ===")
      _     <- Console.print("Player names (comma-separated, min 3): ")
      input <- Console.readLine
      names  = input.split(",").map(_.trim).toList
      _     <- ZIO.fail(new Exception("Need at least 3 players.")).when(names.size < 3)
      state  = GameEngine.newGame(names)
      _     <- gameLoop(state)
    yield ()

  private def gameLoop(state: GameState): Task[Unit] =
    if state.isGameOver then
      val ranked = GameEngine.assignRoles(state.finishOrder, state.players)
      Console.printLine("\n=== Game Over ===") *>
        ZIO.foreach(ranked)(p =>
          Console.printLine(s"${p.role.fold("?")(_.toString)}: ${p.name}")
        ).unit
    else if !state.currentPlayer.hasCards then
      gameLoop(state.copy(currentPlayerIdx = state.nextPlayerIdx))
    else
      for
        _        <- ConsoleIO.printState(state)
        newState <- ConsoleIO.readPlay(state.currentPlayer.hand).flatMap {
                      case None        => ZIO.fromEither(GameEngine.applyPass(state))
                      case Some(cards) => ZIO.fromEither(GameEngine.applyPlay(state, cards))
                    }.catchAll { err =>
                      Console.printLine(s"Invalid move: $err").orDie *> ZIO.succeed(state)
                    }
        _        <- gameLoop(newState)
      yield ()
