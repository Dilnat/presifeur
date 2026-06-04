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
  private val serverUrl =
    val loc = dom.window.location
    if loc.protocol == "file:" then
      "ws://localhost:8080/game"
    else
      val wsProtocol = if loc.protocol == "https:" then "wss:" else "ws:"
      s"$wsProtocol//${loc.host}/game"
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
    "Une version par navigateur du jeu de cartes, avec Scala.js et three.js."
  )
  private val status = div("status")
  private val overlay = div("overlay")
  private val overlayCard = div("overlay-card")
  private val cancelButton = button("Annuler")
  private val playerNameInput = inputText("")
  private val connectButton = button("Se connecter")
  private val errorBox = div("error")
  private val leftPanel = div("panel")
  private val rightPanel = div("panel")
  private val tableArea = div("table-stage")
  private val playersArea = div("players-list")
  private val handArea = div("hand-list")
  private val playButton = button("Jouer les cartes sélectionnées")
  private val passButton = button("Passer")
  private val startButton = button("Commencer la partie")
  private val newGameButton = button("Se déconnecter")
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
    overlayCard.appendChild(h2("Rejoindre la partie"))
    overlayCard.appendChild(
      p(
        "Connectez-vous au salon de jeu. Le premier joueur devient le maître et lance la partie."
      )
    )
    overlayCard.appendChild(playerNameInput)
    overlayCard.appendChild(actionRow(connectButton, cancelButton))
    overlayCard.appendChild(errorBox)

  private def wireActions(): Unit =
    connectButton.onclick = _ =>
      val name = playerNameInput.value.trim
      if name.isEmpty then showError("Entrez un nom de joueur.")
      else
        clearError()
        socket.connect(name)
        localPlayerName = Some(name)

    cancelButton.onclick = _ =>
      overlay.classList.remove("show")
      if remoteState.isEmpty && waitingState.isEmpty then
        status.textContent = "Pas de connexion au serveur."

    playButton.onclick = _ => playSelected()
    passButton.onclick = _ => passTurn()
    startButton.onclick = _ => startGame()
    newGameButton.onclick = _ =>
      socket.disconnect()
      overlay.classList.add("show")
      clearError()

  private def playSelected(): Unit =
    remoteState match
      case None => status.textContent = "Connectez-vous d'abord à un serveur."
      case Some(state) if !state.isYourTurn =>
        status.textContent = "Attendez votre tour."
      case Some(_) if selectedRemoteCards.isEmpty =>
        status.textContent = "Sélectionnez d'abord une ou plusieurs cartes."
      case Some(_) =>
        socket.sendPlay(selectedRemoteCards.toList)
        selectedRemoteCards = Set.empty
        render()

  private def passTurn(): Unit =
    remoteState match
      case None => status.textContent = "Connectez-vous d'abord à un serveur."
      case Some(state) if !state.isYourTurn =>
        status.textContent = "Attendez votre tour."
      case Some(_) =>
        socket.sendPass()

  private def startGame(): Unit =
    waitingState match
      case Some(waiting) if waiting.isMaster && !waiting.isPlaying =>
        socket.sendStart()
      case Some(_) =>
        status.textContent = "Seul le maître du salon peut lancer la partie."
      case None =>
        status.textContent = "Connectez-vous d'abord à un serveur."

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
    status.textContent = "En attente de joueurs..."
    render()

  def onStateReceived(state: RemoteState): Unit =
    remoteState = Some(state)
    waitingState = None
    remoteRankings = None
    exchangeState = None
    selectedRemoteCards = Set.empty
    status.textContent = "Partie en cours."
    render()

  def onGameOverReceived(rankings: List[RankingEntry], isMasterUser: Boolean): Unit =
    remoteRankings = Some(rankings)
    isMaster = isMasterUser
    waitingState = None
    remoteState = None
    exchangeState = None
    selectedRemoteCards = Set.empty
    status.textContent = "Partie terminée."
    render()

  def onExchangeReceived(state: ExchangeState): Unit =
    exchangeState = Some(state)
    waitingState = None
    remoteState = None
    remoteRankings = None
    selectedRemoteCards = Set.empty
    status.textContent = s"Phase d'échange : vous êtes ${formatRole(state.role)}"
    render()

  def onErrorReceived(message: String): Unit =
    status.textContent = message

  def onDisconnected(): Unit =
    resetRemoteState()
    status.textContent = "Déconnecté du serveur."
    render()

  private def formatRole(role: String): String =
    role match
      case "Président"      => "Présifeur"
      case "Trouduc"        => "Troudufeur"
      case "Vice-Président" => "Vice-Présifeur"
      case "Vice-Trouduc"   => "Vice-Troudufeur"
      case other            => other

  private def resetRemoteState(): Unit =
    waitingState = None
    remoteState = None
    remoteRankings = None
    selectedRemoteCards = Set.empty
    localPlayerName = None

  private def render(): Unit =
    renderRemote()

  private def renderRemote(): Unit =
    newGameButton.textContent = "Se déconnecter"
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
          <div class="player-name-text offline">Non connecté</div>
          <div class="player-status-dot offline"></div>
        """
    (waitingState, remoteState, remoteRankings, exchangeState) match
      case (Some(waiting), _, _, _) =>
        clearThree()
        turnLabel.textContent =
          if waiting.isPlaying then s"En attente de ${waiting.master}"
          else s"Salon - ${waiting.master} est le maître"
        tableLabel.textContent = "Table : en attente"
        selectedLabel.textContent = "Sélection : aucune"
        playersArea.innerHTML = waiting.players
          .map(name => s"<div class='player'>$name</div>")
          .mkString
        handArea.innerHTML =
          if waiting.isPlaying then
            s"<div class='muted'>Partie en cours. Vous rejoindrez la suivante.</div>"
          else
            val readiness =
              if waiting.needed > 0 then
                s"Besoin de ${waiting.needed} joueur(s) supplémentaire(s)."
              else "Prêt à commencer."
            if waiting.isMaster then
              s"<div class='muted'>$readiness Vous contrôlez le lancement.</div>"
            else
              s"<div class='muted'>$readiness En attente du lancement par ${waiting.master}.</div>"
        tableArea.innerHTML =
          "<div class='table-text muted'>En attente de joueurs...</div>"
        playButton.disabled = true
        passButton.disabled = true
        startButton.textContent = "Commencer la partie"
        startButton.disabled = !waiting.isMaster || waiting.isPlaying
        startButton.onclick = _ => startGame()
      case (_, _, _, Some(exchange)) =>
        clearThree()
        tableArea.innerHTML = s"""
          <div class='exchange-banner'>
            <h2>Phase d'échange</h2>
            <p>En attente de la fin de l'échange de cartes...</p>
          </div>
        """
        turnLabel.textContent = "Phase d'échange"
        tableLabel.textContent = s"Donner des cartes à : ${formatRole(exchange.target)}"
        selectedLabel.textContent =
          s"Sélection : ${selectedRemoteCards.toList.sorted.mkString(", ")}"
        if exchange.isYourTurn then
          playersArea.innerHTML = s"""
            <div class='exchange-info'>
              <h4>Vous êtes le <strong>${formatRole(exchange.role)}</strong></h4>
              <p>Sélectionnez exactement <strong>${exchange.count}</strong> carte(s) à donner au <strong>${formatRole(exchange.target)}</strong>.</p>
            </div>
          """
          renderExchangeHand(exchange)
          playButton.disabled = selectedRemoteCards.size != exchange.count
          playButton.textContent = s"Donner ${exchange.count} carte(s)"
          playButton.onclick = _ => {
            socket.sendExchange(selectedRemoteCards.toList)
            selectedRemoteCards = Set.empty
            render()
          }
        else
          val infoText =
            if exchange.role == "Trouduc" then
              "Vos 2 meilleures cartes ont été données automatiquement au Présifeur. En attente du choix du Présifeur..."
            else if exchange.role == "Vice-Trouduc" then
              "Votre meilleure carte a été donnée automatiquement au Vice-Présifeur. En attente du choix du Vice-Présifeur..."
            else
              "Les Présifeurs choisissent les cartes à échanger..."
          playersArea.innerHTML = s"""
            <div class='exchange-info waiting-mode'>
              <h4>Phase d'échange</h4>
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
          playButton.textContent = "Confirmer l'échange"
        passButton.disabled = true
        startButton.textContent = "Commencer la partie"
        startButton.disabled = true
      case (_, _, Some(rankings), _) =>
        clearThree()
        turnLabel.textContent = "Partie terminée"
        tableLabel.textContent = "Table : vide"
        selectedLabel.textContent = "Sélection : aucune"
        val rankHtml = rankings.zipWithIndex.map { case (r, idx) =>
          val roleClass = r.role match {
            case "Président"      => "role-president"
            case "Vice-Président" => "role-vp"
            case "Trouduc"        => "role-trouduc"
            case "Vice-Trouduc"   => "role-vt"
            case _                => "role-neutre"
          }
          val displayedRole = formatRole(r.role)
          val rankNumber = idx + 1
          val medal = rankNumber match {
            case 1 => "🥇"
            case 2 => "🥈"
            case 3 => "🥉"
            case _ => s"&nbsp;$rankNumber&nbsp;"
          }
          s"""<div class='ranking-item $roleClass'>
               <span class='rank-medal'>$medal</span>
               <span class='role-badge'>$displayedRole</span>
               <span class='player-name'>${r.name}</span>
             </div>"""
        }.mkString
        playersArea.innerHTML =
          s"<h3>Classement</h3><div class='rankings-list'>$rankHtml</div>"
        handArea.innerHTML =
          if isMaster then
            s"<div class='muted'>Cliquez sur 'Lancer la partie suivante' pour démarrer la phase d'échange.</div>"
          else
            s"<div class='muted'>En attente du lancement de la partie suivante par le maître...</div>"
        tableArea.innerHTML = s"""
          <div class='game-over-banner'>
            <h2>Partie terminée</h2>
            <p>Les rôles ont été attribués pour la partie suivante :</p>
            <div class='rankings-main-list'>
              $rankHtml
            </div>
          </div>
        """
        playButton.disabled = true
        passButton.disabled = true
        startButton.textContent = "Lancer la partie suivante"
        startButton.disabled = !isMaster
        startButton.onclick = _ => socket.sendStart()
      case (_, Some(state), _, _) =>
        turnLabel.textContent = s"Tour : ${state.currentPlayer}"
        tableLabel.textContent =
          state.table.fold("Table : vide")(t => s"Table : $t")
        selectedLabel.textContent =
          if selectedRemoteCards.isEmpty then "Sélection : aucune"
          else s"Sélection : ${selectedRemoteCards.toList.sorted.mkString(", ")}"
        playersArea.innerHTML = state.players.map { p =>
          val current = if p.isCurrentPlayer then " current" else ""
          val roleClass = p.role.fold("") {
            case "Président"      => " role-president"
            case "Vice-Président" => " role-vp"
            case "Trouduc"        => " role-trouduc"
            case "Vice-Trouduc"   => " role-vt"
            case _                => " role-neutre"
          }
          val badge = p.role.fold("") { r =>
            val formatted = formatRole(r)
            s" <span class='role-badge' style='margin-left: 8px;'>$formatted</span>"
          }
          s"<div class='player$current$roleClass'>${p.name}$badge - ${p.cardCount} carte(s)</div>"
        }.mkString
        renderRemoteHand(state)
        renderRemoteThree(state)
        playButton.disabled = selectedRemoteCards.isEmpty || !state.isYourTurn
        playButton.textContent = "Jouer les cartes sélectionnées"
        playButton.onclick = _ => playSelected()
        passButton.disabled = !state.isYourTurn
        startButton.disabled = true
      case _ =>
        clearThree()
        turnLabel.textContent = "En attente du serveur"
        tableLabel.textContent = "Table : vide"
        selectedLabel.textContent = "Sélection : aucune"
        playersArea.innerHTML = ""
        handArea.innerHTML = ""
        tableArea.innerHTML =
          "<div class='table-text muted'>Connectez-vous à un serveur pour commencer.</div>"
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
