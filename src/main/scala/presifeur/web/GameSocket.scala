package presifeur.web

import org.scalajs.dom
import scala.scalajs.js

final class GameSocket(app: BrowserApp, serverUrl: String):

  private var socket: Option[dom.WebSocket] = None

  def connect(name: String): Unit =
    disconnect()
    app.onConnectionStatus(s"Connexion à $serverUrl...")
    val ws = new dom.WebSocket(serverUrl)
    socket = Some(ws)
    ws.onopen = _ =>
      app.onConnectionStatus("Connecté. Entrée dans le salon...")
      sendJoin(name)
      app.onConnected()
    ws.onmessage = (event: dom.MessageEvent) =>
      handleServerMessage(event.data.toString)
    ws.onerror = _ =>
      app.onConnectionStatus("Erreur de connexion.")
    ws.onclose = _ =>
      if socket.contains(ws) then
        socket = None
        app.onDisconnected()

  def disconnect(): Unit =
    socket.foreach(_.close())
    socket = None
    app.onDisconnected()

  def sendJoin(name: String): Unit =
    sendJson(js.Dynamic.literal(tag = "join", name = name))

  def sendPlay(cards: List[String]): Unit =
    sendJson(js.Dynamic.literal(tag = "play", cards = js.Array(cards*)))

  def sendPass(): Unit =
    sendJson(js.Dynamic.literal(tag = "pass"))

  def sendStart(): Unit =
    sendJson(js.Dynamic.literal(tag = "start"))

  def sendExchange(cards: List[String]): Unit =
    sendJson(js.Dynamic.literal(tag = "exchange", cards = js.Array(cards*)))

  private def sendJson(value: js.Any): Unit =
    socket.foreach(_.send(js.JSON.stringify(value)))

  private def handleServerMessage(raw: String): Unit =
    val msg = js.JSON.parse(raw).asInstanceOf[js.Dynamic]
    val tag = msg.tag.asInstanceOf[String]
    tag match
      case "waiting" =>
        app.onWaitingReceived(parseWaiting(msg))
      case "state" =>
        app.onStateReceived(parseState(msg))
      case "gameOver" =>
        val rankings = parseRankings(msg)
        val isMaster = msg.isMaster.asInstanceOf[Boolean]
        app.onGameOverReceived(rankings, isMaster)
      case "exchange" =>
        app.onExchangeReceived(parseExchange(msg))
      case "error" =>
        val message = msg.message.asInstanceOf[String]
        app.onErrorReceived(message)
      case _ =>
        app.onErrorReceived("Message du serveur inconnu.")

  private def parseWaiting(msg: js.Dynamic): WaitingState =
    val master = msg.master.asInstanceOf[String]
    val players = msg.players.asInstanceOf[js.Array[String]].toList
    val needed = msg.needed.asInstanceOf[Int]
    val isMaster = msg.isMaster.asInstanceOf[Boolean]
    val canStart = msg.canStart.asInstanceOf[Boolean]
    val isPlaying = msg.isPlaying.asInstanceOf[Boolean]
    WaitingState(master, players, needed, isMaster, canStart, isPlaying)

  private def parseState(msg: js.Dynamic): RemoteState =
    val hand = msg.hand.asInstanceOf[js.Array[String]].toList
    val table =
      if js.isUndefined(msg.table) || msg.table == null then None
      else Some(msg.table.asInstanceOf[String])
    val tableCards =
      if js.isUndefined(msg.tableCards) || msg.tableCards == null then Nil
      else msg.tableCards.asInstanceOf[js.Array[String]].toList
    val currentPlayer = msg.currentPlayer.asInstanceOf[String]
    val isYourTurn = msg.isYourTurn.asInstanceOf[Boolean]
    val players =
      msg.players.asInstanceOf[js.Array[js.Dynamic]].toList.map { p =>
        RemotePlayer(
          name = p.name.asInstanceOf[String],
          cardCount = p.cardCount.asInstanceOf[Int],
          isCurrentPlayer = p.isCurrentPlayer.asInstanceOf[Boolean],
          role = if js.isUndefined(p.role) || p.role == null then None else Some(p.role.asInstanceOf[String])
        )
      }
    val round = msg.round.asInstanceOf[Int]
    RemoteState(
      hand,
      table,
      tableCards,
      currentPlayer,
      isYourTurn,
      players,
      round
    )

  private def parseRankings(msg: js.Dynamic): List[RankingEntry] =
    msg.rankings.asInstanceOf[js.Array[js.Dynamic]].toList.map { r =>
      RankingEntry(r.role.asInstanceOf[String], r.name.asInstanceOf[String])
    }

  private def parseExchange(msg: js.Dynamic): ExchangeState =
    ExchangeState(
      role = msg.role.asInstanceOf[String],
      target = msg.target.asInstanceOf[String],
      count = msg.count.asInstanceOf[Int],
      isYourTurn = msg.isYourTurn.asInstanceOf[Boolean],
      hand = msg.hand.asInstanceOf[js.Array[String]].toList
    )
