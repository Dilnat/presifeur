package presifeur.model

case class GameState(
  players: Vector[Player],
  currentPlayerIdx: Int,
  lastPlay: Option[Play],
  passCount: Int,
  finishOrder: List[Int],
  round: Int,
  sameRankStreak: Int = 0,
  autoTrouduc: List[Int] = Nil,
  currentRankCount: Int = 0   // total de cartes du rang actuel jouées dans la manche
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
  def forcedRank: Option[Rank] =
    if sameRankStreak >= 2 then lastPlay.map(_.rank) else None
