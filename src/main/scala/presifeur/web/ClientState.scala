package presifeur.web

case class WaitingState(
    master: String,
    players: List[String],
    needed: Int,
    isMaster: Boolean,
    canStart: Boolean,
    isPlaying: Boolean
)

case class RemotePlayer(
    name: String,
    cardCount: Int,
    isCurrentPlayer: Boolean,
    role: Option[String] = None
)

case class RemoteState(
    hand: List[String],
    table: Option[String],
    tableCards: List[String],
    currentPlayer: String,
    isYourTurn: Boolean,
    players: List[RemotePlayer],
    round: Int
)

case class ExchangeState(
    role: String,
    target: String,
    count: Int,
    isYourTurn: Boolean,
    hand: List[String]
)

case class RankingEntry(role: String, name: String)
