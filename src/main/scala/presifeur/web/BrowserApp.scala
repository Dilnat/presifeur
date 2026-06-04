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

  private var threeScene: Option[ThreeScene] = None
  private val serverUrl = "ws://localhost:8080/game"
  private val socket = new GameSocket(this, serverUrl)

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
        localPlayerName = Some(name)
        socket.connect(name)

    cancelButton.onclick = _ =>
      overlay.classList.remove("show")
      if remoteState.isEmpty && waitingState.isEmpty then
        status.textContent = "No server connection."

    playButton.onclick = _ => playSelected()
    passButton.onclick = _ => passTurn()
    startButton.onclick = _ => startGame()
    newGameButton.onclick = _ =>
      socket.disconnect()
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
        socket.sendPlay(selectedRemoteCards.toList)
        selectedRemoteCards = Set.empty
        render()

  private def passTurn(): Unit =
    remoteState match
      case None => status.textContent = "Connect to a server first."
      case Some(state) if !state.isYourTurn =>
        status.textContent = "Wait for your turn."
      case Some(_) =>
        socket.sendPass()

  private def startGame(): Unit =
    waitingState match
      case Some(waiting) if waiting.isMaster && !waiting.isPlaying =>
        socket.sendStart()
      case Some(_) =>
        status.textContent = "Only the master can start the game."
      case None =>
        status.textContent = "Connect to a server first."

  def onConnectionStatus(message: String): Unit =
    status.textContent = message

  def onConnected(): Unit =
    overlay.classList.remove("show")
    render()

  def onWaitingReceived(state: WaitingState): Unit =
    waitingState = Some(state)
    isMaster = state.isMaster
    remoteState = None
    remoteRankings = None
    exchangeState = None
    selectedRemoteCards = Set.empty
    status.textContent = "Waiting for more players..."
    render()

  def onStateReceived(state: RemoteState): Unit =
    remoteState = Some(state)
    waitingState = None
    remoteRankings = None
    exchangeState = None
    selectedRemoteCards = Set.empty
    status.textContent = "Game in progress."
    render()

  def onGameOverReceived(rankings: List[RankingEntry], isMasterUser: Boolean): Unit =
    remoteRankings = Some(rankings)
    isMaster = isMasterUser
    waitingState = None
    remoteState = None
    exchangeState = None
    selectedRemoteCards = Set.empty
    status.textContent = "Game over."
    render()

  def onExchangeReceived(state: ExchangeState): Unit =
    exchangeState = Some(state)
    waitingState = None
    remoteState = None
    remoteRankings = None
    selectedRemoteCards = Set.empty
    status.textContent = s"Exchange phase: you are ${state.role}"
    render()

  def onErrorReceived(message: String): Unit =
    status.textContent = message

  def onDisconnected(): Unit =
    resetRemoteState()
    status.textContent = "Disconnected from server."
    render()

  private def resetRemoteState(): Unit =
    waitingState = None
    remoteState = None
    remoteRankings = None
    selectedRemoteCards = Set.empty
    localPlayerName = None

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
            socket.sendExchange(selectedRemoteCards.toList)
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
        startButton.onclick = _ => socket.sendStart()
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
