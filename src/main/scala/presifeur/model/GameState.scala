package presifeur.model

case class GameState(
  players: Vector[Player],
  currentPlayerIdx: Int,
  lastPlay: Option[Play],
  passCount: Int,          // consecutive passes since last play
  finishOrder: List[Int],  // player ids in finish order
  round: Int
):
  def currentPlayer: Player = players(currentPlayerIdx)
  def activePlayers: Vector[Player] = players.filter(_.hasCards)
  def isRoundOver: Boolean  = activePlayers.size <= 1
  def isGameOver: Boolean   = activePlayers.size == 0
  def nextPlayerIdx: Int    =
    val active = players.indices.filter(players(_).hasCards)
    active.dropWhile(_ <= currentPlayerIdx).headOption
      .orElse(active.headOption)
      .getOrElse(currentPlayerIdx)
