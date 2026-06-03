package presifeur.web

import presifeur.model.*
import presifeur.model.given

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, MessageEvent, document, html}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object BrowserApp:

  def mount(): Unit =
    val app = new BrowserApp()
    app.mount()

private final class BrowserApp:

  private case class WaitingState(
      master: String,
      players: List[String],
      needed: Int,
      isMaster: Boolean,
      canStart: Boolean,
      isPlaying: Boolean
  )
  private case class RemotePlayer(
      name: String,
      cardCount: Int,
      isCurrentPlayer: Boolean
  )
  private case class RemoteState(
      hand: List[String],
      table: Option[String],
      tableCards: List[String],
      currentPlayer: String,
      isYourTurn: Boolean,
      players: List[RemotePlayer],
      round: Int
  )
  private case class ExchangeState(
      role: String,
      target: String,
      count: Int,
      isYourTurn: Boolean,
      hand: List[String]
  )
  private case class RankingEntry(role: String, name: String)

  private var threeScene: Option[ThreeScene] = None
  private var socket: Option[dom.WebSocket] = None
  private var waitingState: Option[WaitingState] = None
  private var remoteState: Option[RemoteState] = None
  private var exchangeState: Option[ExchangeState] = None
  private var remoteRankings: Option[List[RankingEntry]] = None
  private var selectedRemoteCards: Set[String] = Set.empty
  private var isMaster: Boolean = false
  private var localPlayerName: Option[String] = None

  private val root = div("app-root")
  private val header = div("hero")
  private val title = h1("Présifeur")
  private val subtitle = p(
    "A Scala.js + three.js browser version of the card game."
  )
  private val status = div("status")
  private val overlay = div("overlay")
  private val overlayCard = div("overlay-card")
  private val cancelButton = button("Cancel")
  private val serverUrl = "ws://localhost:8080/game"
  private val playerNameInput = inputText("")
  private val connectButton = button("Connect")
  private val errorBox = div("error")
  private val leftPanel = div("panel")
  private val rightPanel = div("panel")
  private val tableArea = div("table-stage")
  private val playersArea = div("players-list")
  private val handArea = div("hand-list")
  private val playButton = button("Play selected")
  private val passButton = button("Pass")
  private val startButton = button("Start game")
  private val newGameButton = button("Disconnect")
  private val playerTitle = div("player-title-container")
  private val turnLabel = div("turn-label")
  private val tableLabel = div("table-label")
  private val selectedLabel = div("selected-label")

  def mount(): Unit =
    document.body.innerHTML = ""
    document.body.appendChild(root)
    setupStyles()
    buildLayout()
    installOverlay()
    wireActions()
    overlay.classList.add("show")
    render()

  private def buildLayout(): Unit =
    header.appendChild(title)
    header.appendChild(subtitle)
    header.appendChild(status)
    root.appendChild(header)

    leftPanel.appendChild(playerTitle)
    leftPanel.appendChild(turnLabel)
    leftPanel.appendChild(tableLabel)
    leftPanel.appendChild(selectedLabel)
    leftPanel.appendChild(playersArea)
    leftPanel.appendChild(handArea)
    leftPanel.appendChild(
      actionRow(playButton, passButton, startButton, newGameButton)
    )

    rightPanel.appendChild(tableArea)

    root.appendChild(leftPanel)
    root.appendChild(rightPanel)
    root.appendChild(overlay)

  private def installOverlay(): Unit =
    overlay.appendChild(overlayCard)
    overlayCard.appendChild(h2("Join the game"))
    overlayCard.appendChild(
      p(
        "Connect to the shared server lobby. The first player becomes the master and starts the game."
      )
    )
    overlayCard.appendChild(playerNameInput)
    overlayCard.appendChild(actionRow(connectButton, cancelButton))
    overlayCard.appendChild(errorBox)

  private def wireActions(): Unit =
    connectButton.onclick = _ =>
      val name = playerNameInput.value.trim
      if name.isEmpty then showError("Enter a player name.")
      else
        clearError()
        connectToServer(name)

    cancelButton.onclick = _ =>
      overlay.classList.remove("show")
      if remoteState.isEmpty && waitingState.isEmpty then
        status.textContent = "No server connection."

    playButton.onclick = _ => playSelected()
    passButton.onclick = _ => passTurn()
    startButton.onclick = _ => startGame()
    newGameButton.onclick = _ =>
      disconnect()
      overlay.classList.add("show")
      clearError()

  private def playSelected(): Unit =
    remoteState match
      case None => status.textContent = "Connect to a server first."
      case Some(state) if !state.isYourTurn =>
        status.textContent = "Wait for your turn."
      case Some(_) if selectedRemoteCards.isEmpty =>
        status.textContent = "Select one or more cards first."
      case Some(_) =>
        sendPlay(selectedRemoteCards.toList)
        selectedRemoteCards = Set.empty
        render()

  private def passTurn(): Unit =
    remoteState match
      case None => status.textContent = "Connect to a server first."
      case Some(state) if !state.isYourTurn =>
        status.textContent = "Wait for your turn."
      case Some(_) =>
        sendPass()

  private def startGame(): Unit =
    waitingState match
      case Some(waiting) if waiting.isMaster && !waiting.isPlaying =>
        sendStart()
      case Some(_) =>
        status.textContent = "Only the master can start the game."
      case None =>
        status.textContent = "Connect to a server first."

  private def render(): Unit =
    renderRemote()

  private def renderRemote(): Unit =
    newGameButton.textContent = "Disconnect"
    def clearThree(): Unit =
      tableArea.innerHTML = ""
      threeScene = None

    localPlayerName match
      case Some(name) =>
        val initial =
          if name.nonEmpty then name.substring(0, 1).toUpperCase else "?"
        playerTitle.innerHTML = s"""
          <div class="player-avatar">$initial</div>
          <div class="player-name-text">$name</div>
          <div class="player-status-dot connected"></div>
        """
      case None =>
        playerTitle.innerHTML = s"""
          <div class="player-avatar offline">?</div>
          <div class="player-name-text offline">Not Connected</div>
          <div class="player-status-dot offline"></div>
        """
    (waitingState, remoteState, remoteRankings, exchangeState) match
      case (Some(waiting), _, _, _) =>
        clearThree()
        turnLabel.textContent =
          if waiting.isPlaying then s"Waiting for ${waiting.master}"
          else s"Lobby - ${waiting.master} is master"
        tableLabel.textContent = "Table: waiting"
        selectedLabel.textContent = "Selected: none"
        playersArea.innerHTML = waiting.players
          .map(name => s"<div class='player'>$name</div>")
          .mkString
        handArea.innerHTML =
          if waiting.isPlaying then
            s"<div class='muted'>Game in progress. You will join the next one.</div>"
          else
            val readiness =
              if waiting.needed > 0 then
                s"Need ${waiting.needed} more player(s)."
              else "Ready to start."
            if waiting.isMaster then
              s"<div class='muted'>$readiness You control the start.</div>"
            else
              s"<div class='muted'>$readiness Waiting for ${waiting.master} to start.</div>"
        tableArea.innerHTML =
          "<div class='table-text muted'>Waiting for players...</div>"
        playButton.disabled = true
        passButton.disabled = true
        startButton.textContent = "Start game"
        startButton.disabled = !waiting.isMaster || waiting.isPlaying
        startButton.onclick = _ => startGame()
      case (_, _, _, Some(exchange)) =>
        clearThree()
        tableArea.innerHTML = s"""
          <div class='exchange-banner'>
            <h2>Exchange Phase</h2>
            <p>Waiting for the exchange of cards to complete...</p>
          </div>
        """
        turnLabel.textContent = "Exchange phase"
        tableLabel.textContent = s"Giving cards to: ${exchange.target}"
        selectedLabel.textContent =
          s"Selected: ${selectedRemoteCards.toList.sorted.mkString(", ")}"
        if exchange.isYourTurn then
          playersArea.innerHTML = s"""
            <div class='exchange-info'>
              <h4>You are the <strong>${exchange.role}</strong></h4>
              <p>Select exactly <strong>${exchange.count}</strong> cards to give to the <strong>${exchange.target}</strong>.</p>
            </div>
          """
          renderExchangeHand(exchange)
          playButton.disabled = selectedRemoteCards.size != exchange.count
          playButton.textContent = s"Give ${exchange.count} cards"
          playButton.onclick = _ => {
            sendExchange(selectedRemoteCards.toList)
            selectedRemoteCards = Set.empty
            render()
          }
        else
          val infoText =
            if exchange.role == "Trouduc" then
              "Your 2 best cards were automatically given to the President. Waiting for the President's choice..."
            else "The President is choosing 2 cards to give to the Trouduc..."
          playersArea.innerHTML = s"""
            <div class='exchange-info waiting-mode'>
              <h4>Exchange phase</h4>
              <p>$infoText</p>
            </div>
          """
          handArea.innerHTML = ""
          exchange.hand.foreach { card =>
            val btn = button(card, classes = List("card-button"))
            btn.disabled = true
            handArea.appendChild(btn)
          }
          playButton.disabled = true
          playButton.textContent = "Confirm exchange"
        passButton.disabled = true
        startButton.textContent = "Start game"
        startButton.disabled = true
      case (_, _, Some(rankings), _) =>
        clearThree()
        turnLabel.textContent = "Game over"
        tableLabel.textContent = "Table: cleared"
        selectedLabel.textContent = "Selected: none"
        val rankHtml = rankings.zipWithIndex.map { case (r, idx) =>
          val roleClass = r.role match {
            case "Président"      => "role-president"
            case "Vice-Président" => "role-vp"
            case "Trouduc"        => "role-trouduc"
            case "Vice-Trouduc"   => "role-vt"
            case _                => "role-neutre"
          }
          val rankNumber = idx + 1
          val medal = rankNumber match {
            case 1 => "🥇"
            case 2 => "🥈"
            case 3 => "🥉"
            case _ => s"&nbsp;$rankNumber&nbsp;"
          }
          s"""<div class='ranking-item $roleClass'>
               <span class='rank-medal'>$medal</span>
               <span class='role-badge'>${r.role}</span>
               <span class='player-name'>${r.name}</span>
             </div>"""
        }.mkString
        playersArea.innerHTML =
          s"<h3>Rankings</h3><div class='rankings-list'>$rankHtml</div>"
        handArea.innerHTML =
          if isMaster then
            s"<div class='muted'>Click 'Start next game' to start the exchange phase.</div>"
          else
            s"<div class='muted'>Waiting for the master to start the next game...</div>"
        tableArea.innerHTML = s"""
          <div class='game-over-banner'>
            <h2>Game Over</h2>
            <p>The rankings have been assigned for the next game:</p>
            <div class='rankings-main-list'>
              $rankHtml
            </div>
          </div>
        """
        playButton.disabled = true
        passButton.disabled = true
        startButton.textContent = "Start next game"
        startButton.disabled = !isMaster
        startButton.onclick = _ => sendStart()
      case (_, Some(state), _, _) =>
        turnLabel.textContent = s"Turn: ${state.currentPlayer}"
        tableLabel.textContent =
          state.table.fold("Table: empty")(t => s"Table: $t")
        selectedLabel.textContent =
          if selectedRemoteCards.isEmpty then "Selected: none"
          else s"Selected: ${selectedRemoteCards.toList.sorted.mkString(", ")}"
        playersArea.innerHTML = state.players.map { p =>
          val current = if p.isCurrentPlayer then " current" else ""
          s"<div class='player$current'>${p.name} - ${p.cardCount} cards</div>"
        }.mkString
        renderRemoteHand(state)
        renderRemoteThree(state)
        playButton.disabled = selectedRemoteCards.isEmpty || !state.isYourTurn
        playButton.textContent = "Play selected"
        playButton.onclick = _ => playSelected()
        passButton.disabled = !state.isYourTurn
        startButton.disabled = true
      case _ =>
        clearThree()
        turnLabel.textContent = "Waiting for server"
        tableLabel.textContent = "Table: empty"
        selectedLabel.textContent = "Selected: none"
        playersArea.innerHTML = ""
        handArea.innerHTML = ""
        tableArea.innerHTML =
          "<div class='table-text muted'>Connect to a server to begin.</div>"
        playButton.disabled = true
        passButton.disabled = true
        startButton.disabled = true

  private def renderExchangeHand(exchange: ExchangeState): Unit =
    handArea.innerHTML = ""
    exchange.hand.foreach { card =>
      val classes =
        List("card-button") ++ (if selectedRemoteCards.contains(card) then
                                  List("selected")
                                else Nil)
      val btn = button(card, classes = classes)
      btn.onclick = _ =>
        if selectedRemoteCards.contains(card) then selectedRemoteCards -= card
        else selectedRemoteCards += card
        render()
      handArea.appendChild(btn)
    }

  private def renderRemoteHand(state: RemoteState): Unit =
    handArea.innerHTML = ""
    state.hand.foreach { card =>
      val classes =
        List("card-button") ++ (if selectedRemoteCards.contains(card) then
                                  List("selected")
                                else Nil)
      val btn = button(card, classes = classes)
      btn.disabled = !state.isYourTurn
      btn.onclick = _ =>
        if state.isYourTurn then
          if selectedRemoteCards.contains(card) then selectedRemoteCards -= card
          else selectedRemoteCards += card
          render()
      handArea.appendChild(btn)
    }

  private def renderRemoteThree(state: RemoteState): Unit =
    val selected = selectedRemoteCards.toList.flatMap(parseCardToken).sorted
    val tableCards = state.tableCards.flatMap(parseCardToken)
    if threeScene.isEmpty then
      tableArea.innerHTML = ""
      threeScene = Some(ThreeScene(tableArea))
    threeScene.foreach(_.updateCards(tableCards, selected, gameOver = false))

  private def connectToServer(name: String): Unit =
    disconnect()
    localPlayerName = Some(name)
    render()
    status.textContent = s"Connecting to ${serverUrl}..."
    val ws = new dom.WebSocket(serverUrl)
    socket = Some(ws)
    ws.onopen = _ =>
      status.textContent = "Connected. Joining lobby..."
      sendJoin(name)
      overlay.classList.remove("show")
      render()
    ws.onmessage = (event: MessageEvent) =>
      handleServerMessage(event.data.toString)
    ws.onerror = _ => status.textContent = "Connection error."
    ws.onclose = _ =>
      if socket.contains(ws) then
        socket = None
        resetRemoteState()
        status.textContent = "Connection closed."
        render()

  private def disconnect(): Unit =
    socket.foreach(_.close())
    socket = None
    resetRemoteState()
    status.textContent = "Disconnected from server."
    render()

  private def resetRemoteState(): Unit =
    waitingState = None
    remoteState = None
    remoteRankings = None
    selectedRemoteCards = Set.empty
    localPlayerName = None

  private def handleServerMessage(raw: String): Unit =
    val msg = js.JSON.parse(raw).asInstanceOf[js.Dynamic]
    val tag = msg.tag.asInstanceOf[String]
    tag match
      case "waiting" =>
        val parsed = parseWaiting(msg)
        waitingState = Some(parsed)
        isMaster = parsed.isMaster
        remoteState = None
        remoteRankings = None
        exchangeState = None
        selectedRemoteCards = Set.empty
        status.textContent = "Waiting for more players..."
        render()
      case "state" =>
        remoteState = Some(parseState(msg))
        waitingState = None
        remoteRankings = None
        exchangeState = None
        selectedRemoteCards = Set.empty
        status.textContent = "Game in progress."
        render()
      case "gameOver" =>
        remoteRankings = Some(parseRankings(msg))
        isMaster = msg.isMaster.asInstanceOf[Boolean]
        waitingState = None
        remoteState = None
        exchangeState = None
        selectedRemoteCards = Set.empty
        status.textContent = "Game over."
        render()
      case "exchange" =>
        exchangeState = Some(parseExchange(msg))
        waitingState = None
        remoteState = None
        remoteRankings = None
        selectedRemoteCards = Set.empty
        status.textContent =
          s"Exchange phase: you are ${exchangeState.get.role}"
        render()
      case "error" =>
        val message = msg.message.asInstanceOf[String]
        status.textContent = message
      case _ =>
        status.textContent = "Unknown server message."

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
          isCurrentPlayer = p.isCurrentPlayer.asInstanceOf[Boolean]
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

  private def sendJoin(name: String): Unit =
    sendJson(js.Dynamic.literal(tag = "join", name = name))

  private def sendPlay(cards: List[String]): Unit =
    sendJson(js.Dynamic.literal(tag = "play", cards = js.Array(cards*)))

  private def sendPass(): Unit =
    sendJson(js.Dynamic.literal(tag = "pass"))

  private def sendStart(): Unit =
    sendJson(js.Dynamic.literal(tag = "start"))

  private def sendExchange(cards: List[String]): Unit =
    sendJson(js.Dynamic.literal(tag = "exchange", cards = js.Array(cards*)))

  private def sendJson(value: js.Any): Unit =
    socket.foreach(_.send(js.JSON.stringify(value)))

  private def showError(message: String): Unit =
    errorBox.textContent = message
    errorBox.className = "error visible"

  private def clearError(): Unit =
    errorBox.textContent = ""
    errorBox.className = "error"

  private def parseCardToken(token: String): Option[Card] =
    if token.length < 2 then None
    else
      val suit = token.last.toString match
        case "♠" | "P" => Some(Suit.Piques)
        case "♥" | "C" => Some(Suit.Coeurs)
        case "♦" | "K" => Some(Suit.Carreaux)
        case "♣" | "T" => Some(Suit.Trefles)
        case _         => None
      val rank = token.dropRight(1).toUpperCase match
        case "3"  => Some(Rank.Trois)
        case "4"  => Some(Rank.Quatre)
        case "5"  => Some(Rank.Cinq)
        case "6"  => Some(Rank.Six)
        case "7"  => Some(Rank.Sept)
        case "8"  => Some(Rank.Huit)
        case "9"  => Some(Rank.Neuf)
        case "10" => Some(Rank.Dix)
        case "V"  => Some(Rank.Valet)
        case "D"  => Some(Rank.Dame)
        case "R"  => Some(Rank.Roi)
        case "A"  => Some(Rank.As)
        case "2"  => Some(Rank.Deux)
        case _    => None
      for r <- rank; su <- suit yield Card(r, su)

  private def setupStyles(): Unit =
    val style = document.createElement("style")
    style.textContent = """
      :root { color-scheme: dark; }
      body { margin: 0; font-family: ui-sans-serif, system-ui, sans-serif; background: radial-gradient(circle at top, #1f3b2b, #08130f 70%); color: #f4f3ee; }
      .app-root { min-height: 100vh; display: grid; grid-template-columns: 360px 1fr; gap: 24px; padding: 24px; box-sizing: border-box; }
      .hero, .panel, .overlay-card { backdrop-filter: blur(16px); background: rgba(12, 18, 15, 0.72); border: 1px solid rgba(255,255,255,0.12); box-shadow: 0 18px 50px rgba(0,0,0,0.35); }
      .hero { grid-column: 1 / -1; padding: 24px; border-radius: 24px; }
      .hero h1 { margin: 0; font-size: 3rem; letter-spacing: 0.08em; text-transform: uppercase; }
      .hero p { margin: 10px 0 0; color: #c8d3c8; }
      .status { margin-top: 16px; color: #ffe8b7; min-height: 1.25rem; }
      .panel { border-radius: 24px; padding: 20px; display: flex; flex-direction: column; gap: 16px; }
      .table-stage { min-height: 720px; border-radius: 24px; overflow: hidden; background: linear-gradient(180deg, rgba(37,82,56,0.2), rgba(4,10,7,0.8)); border: 1px solid rgba(255,255,255,0.10); }
      .players-list, .hand-list { display: flex; flex-direction: column; gap: 8px; }
      .player { padding: 10px 12px; border-radius: 14px; background: rgba(255,255,255,0.05); }
      .player.current { background: rgba(255, 220, 130, 0.16); }
      .player.done { opacity: 0.5; }
      .card-button, button { border: 0; border-radius: 999px; padding: 12px 16px; cursor: pointer; font-weight: 700; }
      .card-button { background: rgba(255,255,255,0.07); color: #f6f5f1; text-align: left; }
      .card-button.selected { background: linear-gradient(135deg, #ffe08a, #ffb35c); color: #21160a; }
      .actions { display: flex; flex-wrap: wrap; gap: 10px; }
      .actions button { background: linear-gradient(135deg, #2d6b4f, #1f4d38); color: white; }
      .actions button:disabled { opacity: 0.45; cursor: not-allowed; }
      .overlay { position: fixed; inset: 0; display: none; align-items: center; justify-content: center; background: rgba(0,0,0,0.6); z-index: 20; padding: 20px; }
      .overlay.show { display: flex; }
      .overlay-card { width: min(560px, 100%); border-radius: 28px; padding: 24px; display: flex; flex-direction: column; gap: 12px; }
      .overlay-card input { border-radius: 16px; border: 1px solid rgba(255,255,255,0.15); background: rgba(255,255,255,0.08); color: #f4f3ee; padding: 14px 16px; font-size: 1rem; }
      .overlay-divider { height: 1px; width: 100%; background: rgba(255,255,255,0.12); margin: 8px 0; }
      .error { min-height: 1.25rem; color: #ff9d9d; }
      .error.visible { font-weight: 700; }
      .muted { color: #aeb8af; }
      .table-text { padding: 24px; font-size: 1.2rem; }
      .three-root { width: 100%; height: 100%; }
      .ranking-item {
        display: flex;
        align-items: center;
        gap: 12px;
        padding: 12px 16px;
        border-radius: 16px;
        background: rgba(255, 255, 255, 0.05);
        border: 1px solid rgba(255, 255, 255, 0.08);
        margin-bottom: 8px;
        font-size: 1.1rem;
        transition: all 0.3s ease;
      }
      .ranking-item:hover {
        transform: translateX(4px);
        background: rgba(255, 255, 255, 0.08);
      }
      .role-badge {
        font-weight: 700;
        font-size: 0.85rem;
        text-transform: uppercase;
        padding: 4px 10px;
        border-radius: 999px;
        letter-spacing: 0.05em;
      }
      .role-president { border-left: 4px solid #ffe08a; }
      .role-president .role-badge { background: #ffe08a; color: #21160a; }
      .role-vp { border-left: 4px solid #ffd0a0; }
      .role-vp .role-badge { background: #ffd0a0; color: #21160a; }
      .role-neutre { border-left: 4px solid #aeb8af; }
      .role-neutre .role-badge { background: rgba(255, 255, 255, 0.15); color: #f4f3ee; }
      .role-vt { border-left: 4px solid #8e8ba0; }
      .role-vt .role-badge { background: #8e8ba0; color: #f4f3ee; }
      .role-trouduc { border-left: 4px solid #ff7a7a; }
      .role-trouduc .role-badge { background: #ff7a7a; color: #21160a; }
      .exchange-info {
        background: rgba(255, 220, 130, 0.08);
        border: 1px solid rgba(255, 220, 130, 0.2);
        padding: 16px;
        border-radius: 16px;
        margin-bottom: 16px;
      }
      .exchange-info h4 {
        margin: 0 0 8px 0;
        color: #ffe08a;
      }
      .exchange-info p {
        margin: 0;
        font-size: 0.95rem;
        line-height: 1.4;
        color: #c8d3c8;
      }
      .exchange-info.waiting-mode {
        background: rgba(255, 255, 255, 0.04);
        border: 1px solid rgba(255, 255, 255, 0.08);
      }
      .exchange-info.waiting-mode h4 {
        color: #ffe8b7;
      }
      .game-over-banner {
        padding: 48px;
        text-align: center;
        background: linear-gradient(135deg, rgba(45,29,21,0.6), rgba(12,18,15,0.8));
        border-radius: 24px;
        margin: 24px;
        border: 1px solid rgba(255, 122, 122, 0.15);
      }
      .game-over-banner h2 {
        font-size: 2.5rem;
        margin: 0 0 16px 0;
        color: #ff7a7a;
        text-transform: uppercase;
        letter-spacing: 0.1em;
      }
      .game-over-banner p {
        color: #c8d3c8;
        font-size: 1.1rem;
      }
      .player-title-container {
        display: flex;
        align-items: center;
        gap: 12px;
        padding: 12px 16px;
        border-radius: 16px;
        background: linear-gradient(135deg, rgba(255, 255, 255, 0.06), rgba(255, 255, 255, 0.02));
        border: 1px solid rgba(255, 255, 255, 0.08);
        box-shadow: inset 0 1px 1px rgba(255,255,255,0.05);
        margin-bottom: 8px;
      }
      .player-avatar {
        width: 36px;
        height: 36px;
        border-radius: 50%;
        background: linear-gradient(135deg, #ffe08a, #ffb35c);
        display: flex;
        align-items: center;
        justify-content: center;
        color: #21160a;
        font-weight: bold;
        font-size: 1.1rem;
        box-shadow: 0 2px 8px rgba(255, 179, 92, 0.3);
      }
      .player-avatar.offline {
        background: linear-gradient(135deg, #595959, #434343);
        color: #aeb8af;
        box-shadow: none;
      }
      .player-name-text {
        font-size: 1.1rem;
        font-weight: 600;
        color: #f4f3ee;
        letter-spacing: 0.02em;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        max-width: 180px;
      }
      .player-name-text.offline {
        color: #aeb8af;
      }
      .player-status-dot {
        width: 8px;
        height: 8px;
        border-radius: 50%;
        margin-left: auto;
      }
      .player-status-dot.connected {
        background-color: #52c41a;
        box-shadow: 0 0 8px #52c41a;
        animation: pulse-green 2s infinite;
      }
      .player-status-dot.offline {
        background-color: #8c8c8c;
        box-shadow: none;
      }
      @keyframes pulse-green {
        0% {
          transform: scale(0.95);
          box-shadow: 0 0 0 0 rgba(82, 196, 26, 0.7);
        }
        70% {
          transform: scale(1);
          box-shadow: 0 0 0 6px rgba(82, 196, 26, 0);
        }
        100% {
          transform: scale(0.95);
          box-shadow: 0 0 0 0 rgba(82, 196, 26, 0);
        }
      }
      .rank-medal {
        font-size: 1.3rem;
        min-width: 24px;
        text-align: center;
      }
      .rankings-main-list {
        margin: 32px auto 0 auto;
        max-width: 480px;
        display: flex;
        flex-direction: column;
        gap: 12px;
        text-align: left;
      }
      .game-over-banner {
        padding: 48px;
        text-align: center;
        background: linear-gradient(135deg, rgba(45,29,21,0.6), rgba(12,18,15,0.8));
        border-radius: 24px;
        margin: 60px auto;
        border: 1px solid rgba(255, 122, 122, 0.15);
        max-width: 640px;
        width: calc(100% - 96px);
        box-sizing: border-box;
      }
      .exchange-banner {
        padding: 48px;
        text-align: center;
        background: linear-gradient(135deg, rgba(37, 56, 82, 0.4), rgba(12, 15, 18, 0.8));
        border-radius: 24px;
        margin: 60px auto;
        border: 1px solid rgba(130, 220, 255, 0.15);
        max-width: 640px;
        width: calc(100% - 96px);
        box-sizing: border-box;
      }
      .exchange-banner h2 {
        font-size: 2.5rem;
        margin: 0 0 16px 0;
        color: #ffe08a;
        text-transform: uppercase;
        letter-spacing: 0.1em;
      }
      .exchange-banner p {
        color: #c8d3c8;
        font-size: 1.1rem;
      }
    """
    document.head.appendChild(style)

  private def div(cls: String): html.Div =
    val el = document.createElement("div").asInstanceOf[html.Div]
    el.className = cls
    el

  private def h1(text: String): html.Heading =
    val el = document.createElement("h1").asInstanceOf[html.Heading]
    el.textContent = text
    el

  private def h2(text: String): html.Heading =
    val el = document.createElement("h2").asInstanceOf[html.Heading]
    el.textContent = text
    el

  private def p(text: String): html.Paragraph =
    val el = document.createElement("p").asInstanceOf[html.Paragraph]
    el.textContent = text
    el

  private def button(text: String, classes: List[String] = Nil): html.Button =
    val el = document.createElement("button").asInstanceOf[html.Button]
    el.textContent = text
    el.className = classes.mkString(" ")
    el

  private def inputText(value: String): html.Input =
    val el = document.createElement("input").asInstanceOf[html.Input]
    el.value = value
    el

  private def actionRow(buttons: html.Button*): html.Div =
    val row = div("actions")
    buttons.foreach(row.appendChild)
    row

private final class ThreeScene(container: html.Div):

  private val three = js.Dynamic.global.THREE
  private val scene =
    js.Dynamic.newInstance(three.Scene)().asInstanceOf[js.Dynamic]
  private val camera = js.Dynamic
    .newInstance(three.PerspectiveCamera)(45, 1.0, 0.1, 1000)
    .asInstanceOf[js.Dynamic]
  private val renderer = js.Dynamic
    .newInstance(three.WebGLRenderer)(
      js.Dynamic.literal(antialias = true, alpha = true)
    )
    .asInstanceOf[js.Dynamic]
  private val cardsLayer =
    js.Dynamic.newInstance(three.Group)().asInstanceOf[js.Dynamic]
  private val tableSurface = js.Dynamic
    .newInstance(three.Mesh)(
      js.Dynamic.newInstance(three.PlaneGeometry)(18, 10),
      js.Dynamic.newInstance(three.MeshBasicMaterial)(
        js.Dynamic.literal(color = 0x123b28, side = three.DoubleSide)
      )
    )
    .asInstanceOf[js.Dynamic]

  private var cardMeshes: List[js.Dynamic] = Nil
  private var running = false

  init()

  private def init(): Unit =
    renderer.setSize(
      container.clientWidth.max(1),
      container.clientHeight.max(1)
    )
    renderer.domElement.classList.add("three-root")
    container.appendChild(renderer.domElement.asInstanceOf[dom.Node])
    scene.background = js.Dynamic.newInstance(three.Color)(0x07110d)
    scene.add(tableSurface)
    scene.add(cardsLayer)
    tableSurface.rotation.x = -Math.PI / 2
    camera.position.set(0, 10, 14)
    camera.lookAt(0, 0, 0)
    resize()
    dom.window.addEventListener("resize", (_: dom.Event) => resize())
    animate()

  private def resize(): Unit =
    val width = container.clientWidth.max(1)
    val height = container.clientHeight.max(1)
    camera.aspect = width.toDouble / height.toDouble
    camera.updateProjectionMatrix()
    renderer.setSize(width, height)

  def update(state: GameState, selected: List[Card], gameOver: Boolean): Unit =
    val tableCards = state.lastPlay.map(_.cards).getOrElse(Nil)
    updateCards(tableCards, selected, gameOver)

  def updateCards(
      tableCards: List[Card],
      selected: List[Card],
      gameOver: Boolean
  ): Unit =
    clearCards()
    val tableStack = tableCards.zipWithIndex
      .map((card, index) =>
        createCardMesh(card, index * 0.18, 0.5, index * 0.04, 0)
      )
      .toList
    val selectedFan = selected.zipWithIndex
      .map((card, index) =>
        createCardMesh(
          card,
          -selected.size * 0.22 + index * 0.44,
          0.25,
          4.2,
          -0.06 + index * 0.01
        )
      )
      .toList
    cardMeshes = tableStack ++ selectedFan
    cardMeshes.foreach(card => cardsLayer.add(card))
    if gameOver then
      tableSurface.material.color =
        js.Dynamic.newInstance(three.Color)(0x2d1d15)
    else
      tableSurface.material.color =
        js.Dynamic.newInstance(three.Color)(0x123b28)

  private def createCardMesh(
      card: Card,
      x: Double,
      y: Double,
      z: Double,
      tilt: Double
  ): js.Dynamic =
    val texture = js.Dynamic
      .newInstance(three.CanvasTexture)(makeCardCanvas(card))
      .asInstanceOf[js.Dynamic]
    val material = js.Dynamic.newInstance(three.MeshBasicMaterial)(
      js.Dynamic.literal(map = texture, transparent = true)
    )
    val mesh = js.Dynamic
      .newInstance(three.Mesh)(
        js.Dynamic.newInstance(three.PlaneGeometry)(1.8, 2.6),
        material
      )
      .asInstanceOf[js.Dynamic]
    mesh.position.set(x, y, z)
    mesh.rotation.x = -Math.PI / 2.0 + 0.03
    mesh.rotation.z = tilt
    mesh

  private def clearCards(): Unit =
    cardMeshes.foreach(card => cardsLayer.remove(card))
    cardMeshes = Nil

  private def animate(): Unit =
    if !running then
      running = true
      var frame: js.Function1[Double, Unit] = null
      frame = (time: Double) =>
        cardsLayer.rotation.y = Math.sin(time / 2200.0) * 0.18
        renderer.render(scene, camera)
        dom.window.requestAnimationFrame(frame)
      dom.window.requestAnimationFrame(frame)

  private def makeCardCanvas(card: Card): dom.html.Canvas =
    val canvas = document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    canvas.width = 320
    canvas.height = 460
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val suitColor = card.suit match
      case Suit.Coeurs | Suit.Carreaux => "#c43131"
      case _                           => "#182029"
    ctx.fillStyle = "#f8f5ea"
    ctx.fillRect(0, 0, canvas.width, canvas.height)
    ctx.strokeStyle = suitColor
    ctx.lineWidth = 10
    ctx.strokeRect(12, 12, canvas.width - 24, canvas.height - 24)
    ctx.fillStyle = suitColor
    ctx.font = "bold 64px Georgia"
    ctx.fillText(card.rank.courte, 24, 84)
    ctx.font = "48px Georgia"
    ctx.fillText(card.suit.symbole, 24, 145)
    ctx.save()
    ctx.translate(canvas.width - 24, canvas.height - 24)
    ctx.rotate(Math.PI)
    ctx.font = "bold 64px Georgia"
    ctx.fillText(card.rank.courte, 0, 0)
    ctx.font = "48px Georgia"
    ctx.fillText(card.suit.symbole, 0, 60)
    ctx.restore()
    ctx.font = "bold 46px Georgia"
    ctx.textAlign = "center"
    ctx.fillText(card.toString, canvas.width / 2, canvas.height / 2 + 18)
    canvas
