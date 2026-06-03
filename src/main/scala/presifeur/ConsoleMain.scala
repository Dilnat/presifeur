package presifeur

import presifeur.engine.GameEngine
import presifeur.io.ConsoleIO
import presifeur.model.*
import zio.*

object ConsoleMain extends ZIOAppDefault:

  override def run: Task[Unit] =
    for
      _     <- Console.printLine("=== Président ===")
      _     <- Console.print("Noms des joueurs (séparés par des virgules, min 3) : ")
      input <- Console.readLine
      names  = input.split(",").map(_.trim).toList
      _     <- (Console.printLine("Il faut au moins 3 joueurs.").orDie *> exit(ExitCode.failure)).when(names.size < 3)
      _     <- playLoop(names, None)
    yield ()

  private def playLoop(names: List[String], prevRanked: Option[Vector[Player]]): Task[Unit] =
    for
      baseState <- ZIO.succeed(GameEngine.newGame(names))
      state     <- prevRanked match
                     case None         => ZIO.succeed(baseState)
                     case Some(ranked) => ConsoleIO.applyExchange(baseState, ranked)
      ranked    <- gameLoop(state)
      _         <- Console.printLine("\n=== Fin de partie ===")
      _         <- ZIO.foreach(ranked)(p =>
                     Console.printLine(s"${p.role.fold("?")(_.nom)} : ${p.name}")
                   )
      _         <- Console.print("\nRejouer ? (o/n) : ").orDie
      again     <- Console.readLine
      _         <- if again.trim.equalsIgnoreCase("o") then playLoop(names, Some(ranked))
                   else ZIO.unit
    yield ()

  private def gameLoop(state: GameState): Task[Vector[Player]] =
    val remaining = state.activePlayers
    if state.isGameOver || remaining.size <= 1 then
      val lastIds    = remaining.map(_.id).filterNot(state.finishOrder.contains)
      val finalOrder = state.finishOrder ++ lastIds
      ZIO.succeed(GameEngine.assignRoles(finalOrder, state.players, state.autoTrouduc))
    else if !state.currentPlayer.hasCards then
      gameLoop(state.copy(currentPlayerIdx = state.nextPlayerIdx))
    else
      for
        _        <- ConsoleIO.printState(state)
        _        <- Console.printLine("Vous n'avez aucune carte jouable, vous devez passer.").orDie
                      .when(!GameEngine.canPlay(state.currentPlayer.hand, state))
        newState <- ConsoleIO.readPlay(state.currentPlayer.hand, state.forcedRank,
                      mustPass = !GameEngine.canPlay(state.currentPlayer.hand, state)).flatMap {
                      case None        => ZIO.fromEither(GameEngine.applyPass(state))
                      case Some(cards) => ZIO.fromEither(GameEngine.applyPlay(state, cards))
                    }.catchAll { err =>
                      Console.printLine(s"Coup invalide : $err").orDie *> ZIO.succeed(state)
                    }
        result   <- gameLoop(newState)
      yield result
