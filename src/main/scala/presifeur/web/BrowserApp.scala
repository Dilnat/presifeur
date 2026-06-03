package presifeur.web

import presifeur.engine.GameEngine
import presifeur.model.*
import presifeur.model.given

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, document, html}
import scala.scalajs.js

object BrowserApp:

  def mount(): Unit =
    val app = new BrowserApp()
    app.mount()

private final class BrowserApp:

  private var gameState: Option[GameState] = None
  private var selectedCards: Set[Card] = Set.empty
  private var cardButtons: Map[Card, html.Button] = Map.empty
  private var threeScene: Option[ThreeScene] = None

  private val root = div("app-root")
  private val header = div("hero")
  private val title = h1("Président")
  private val subtitle = p("A Scala.js + three.js browser version of the card game.")
  private val status = div("status")
  private val overlay = div("overlay")
  private val overlayCard = div("overlay-card")
  private val nameInput = inputText("Alice, Bob, Carol")
  private val startButton = button("Start game")
  private val cancelButton = button("Cancel")
  private val errorBox = div("error")
  private val leftPanel = div("panel")
  private val rightPanel = div("panel")
  private val tableArea = div("table-stage")
  private val playersArea = div("players-list")
  private val handArea = div("hand-list")
  private val playButton = button("Play selected")
  private val passButton = button("Pass")
  private val newGameButton = button("New game")
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

    leftPanel.appendChild(turnLabel)
    leftPanel.appendChild(tableLabel)
    leftPanel.appendChild(selectedLabel)
    leftPanel.appendChild(playersArea)
    leftPanel.appendChild(handArea)
    leftPanel.appendChild(actionRow(playButton, passButton, newGameButton))

    rightPanel.appendChild(tableArea)

    root.appendChild(leftPanel)
    root.appendChild(rightPanel)
    root.appendChild(overlay)

  private def installOverlay(): Unit =
    overlay.appendChild(overlayCard)
    overlayCard.appendChild(h2("Enter player names"))
    overlayCard.appendChild(p("Separate names with commas, minimum 3 players."))
    overlayCard.appendChild(nameInput)
    overlayCard.appendChild(errorBox)
    overlayCard.appendChild(actionRow(startButton, cancelButton))

  private def wireActions(): Unit =
    startButton.onclick = _ =>
      parseNames(nameInput.value) match
        case Left(message) =>
          errorBox.textContent = message
          errorBox.className = "error visible"
        case Right(names) =>
          errorBox.textContent = ""
          errorBox.className = "error"
          startGame(names)

    cancelButton.onclick = _ =>
      overlay.classList.remove("show")
      if gameState.isEmpty then
        status.textContent = "No game started. Reload the page to try again."

    playButton.onclick = _ => playSelected()
    passButton.onclick = _ => passTurn()
    newGameButton.onclick = _ =>
      overlay.classList.add("show")
      errorBox.textContent = ""
      errorBox.className = "error"

  private def startGame(names: List[String]): Unit =
    gameState = Some(GameEngine.newGame(names))
    selectedCards = Set.empty
    overlay.classList.remove("show")
    status.textContent = s"Game started with ${names.mkString(", ")}."
    render()

  private def playSelected(): Unit =
    gameState match
      case None => status.textContent = "Start a game first."
      case Some(state) =>
        val cards = state.currentPlayer.hand.filter(selectedCards.contains)
        if cards.isEmpty then
          status.textContent = "Select one or more cards first."
        else
          GameEngine.applyPlay(state, cards) match
            case Left(error) => status.textContent = error
            case Right(updated) =>
              gameState = Some(updated)
              selectedCards = Set.empty
              render()

  private def passTurn(): Unit =
    gameState match
      case None => status.textContent = "Start a game first."
      case Some(state) =>
        GameEngine.applyPass(state) match
          case Left(error) => status.textContent = error
          case Right(updated) =>
            gameState = Some(updated)
            selectedCards = Set.empty
            render()

  private def render(): Unit =
    cardButtons = Map.empty
    gameState match
      case None =>
        turnLabel.textContent = "Waiting for players"
        tableLabel.textContent = "Table: empty"
        selectedLabel.textContent = "Selected: none"
        playersArea.innerHTML = ""
        handArea.innerHTML = ""
        tableArea.innerHTML = ""
        playButton.disabled = true
        passButton.disabled = true
      case Some(state) if state.isGameOver =>
        renderGameOver(state)
      case Some(state) =>
        if !state.currentPlayer.hasCards then
          gameState = Some(state.copy(currentPlayerIdx = state.nextPlayerIdx))
          render()
        else
          turnLabel.textContent = s"Turn: ${state.currentPlayer.name}"
          tableLabel.textContent = state.lastPlay match
            case None => "Table: empty"
            case Some(play) => s"Table: ${play.rank.courte} x${play.size}"
          selectedLabel.textContent = if selectedCards.isEmpty then "Selected: none" else s"Selected: ${selectedCards.toList.sorted.map(_.toString).mkString(", ")}" 
          renderPlayers(state)
          renderHand(state)
          renderThree(state)
          playButton.disabled = selectedCards.isEmpty
          passButton.disabled = false

  private def renderGameOver(state: GameState): Unit =
    val ranked = GameEngine.assignRoles(state.finishOrder, state.players)
    turnLabel.textContent = "Game over"
    tableLabel.textContent = "Table: cleared"
    selectedLabel.textContent = "Selected: none"
    playersArea.innerHTML = ranked.map(p => s"<div>${p.role.fold("?")(_.nom)}: ${p.name}</div>").mkString
    handArea.innerHTML = "<div class='muted'>No more cards.</div>"
    tableArea.innerHTML = ""
    playButton.disabled = true
    passButton.disabled = true
    status.textContent = "Open New game to play again."
    renderThree(state, gameOver = true)

  private def renderPlayers(state: GameState): Unit =
    playersArea.innerHTML = state.players.map { player =>
      val active = if player.hasCards then "" else " done"
      val role = player.role.fold("")(r => s" [${r.nom}]")
      s"<div class='player$active${if state.currentPlayer.id == player.id then " current" else ""}'>${player.name}$role - ${player.cardCount} cards</div>"
    }.mkString

  private def renderHand(state: GameState): Unit =
    handArea.innerHTML = ""
    cardButtons = state.currentPlayer.hand.map { card =>
      val btn = button(card.toString, classes = List("card-button") ++ (if selectedCards.contains(card) then List("selected") else Nil))
      btn.onclick = _ =>
        if selectedCards.contains(card) then selectedCards -= card
        else selectedCards += card
        render()
      handArea.appendChild(btn)
      card -> btn
    }.toMap

  private def renderThree(state: GameState, gameOver: Boolean = false): Unit =
    if threeScene.isEmpty then
      threeScene = Some(ThreeScene(tableArea))
    threeScene.foreach(_.update(state, selectedCards.toList.sorted, gameOver))

  private def parseNames(raw: String): Either[String, List[String]] =
    val names = raw.split(",").map(_.trim).filter(_.nonEmpty).toList
    if names.size < 3 then Left("Please enter at least 3 names.")
    else Right(names)

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
      .error { min-height: 1.25rem; color: #ff9d9d; }
      .error.visible { font-weight: 700; }
      .muted { color: #aeb8af; }
      .three-root { width: 100%; height: 100%; }
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
  private val scene = js.Dynamic.newInstance(three.Scene)().asInstanceOf[js.Dynamic]
  private val camera = js.Dynamic.newInstance(three.PerspectiveCamera)(45, 1.0, 0.1, 1000).asInstanceOf[js.Dynamic]
  private val renderer = js.Dynamic.newInstance(three.WebGLRenderer)(js.Dynamic.literal(antialias = true, alpha = true)).asInstanceOf[js.Dynamic]
  private val cardsLayer = js.Dynamic.newInstance(three.Group)().asInstanceOf[js.Dynamic]
  private val tableSurface = js.Dynamic.newInstance(three.Mesh)(
    js.Dynamic.newInstance(three.PlaneGeometry)(18, 10),
    js.Dynamic.newInstance(three.MeshBasicMaterial)(js.Dynamic.literal(color = 0x123b28, side = three.DoubleSide))
  ).asInstanceOf[js.Dynamic]

  private var cardMeshes: List[js.Dynamic] = Nil
  private var running = false

  init()

  private def init(): Unit =
    renderer.setSize(container.clientWidth.max(1), container.clientHeight.max(1))
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
    clearCards()
    val tableCards = state.lastPlay.map(_.cards).getOrElse(Nil)
    val tableStack = tableCards.zipWithIndex.map((card, index) => createCardMesh(card, index * 0.18, 0.5, index * 0.04, 0)).toList
    val selectedFan = selected.zipWithIndex.map((card, index) => createCardMesh(card, -selected.size * 0.22 + index * 0.44, 0.25, 4.2, -0.06 + index * 0.01)).toList
    cardMeshes = tableStack ++ selectedFan
    cardMeshes.foreach(card => cardsLayer.add(card))
    if gameOver then
      tableSurface.material.color = js.Dynamic.newInstance(three.Color)(0x2d1d15)
    else
      tableSurface.material.color = js.Dynamic.newInstance(three.Color)(0x123b28)

  private def createCardMesh(card: Card, x: Double, y: Double, z: Double, tilt: Double): js.Dynamic =
    val texture = js.Dynamic.newInstance(three.CanvasTexture)(makeCardCanvas(card)).asInstanceOf[js.Dynamic]
    val material = js.Dynamic.newInstance(three.MeshBasicMaterial)(js.Dynamic.literal(map = texture, transparent = true))
    val mesh = js.Dynamic.newInstance(three.Mesh)(js.Dynamic.newInstance(three.PlaneGeometry)(1.8, 2.6), material).asInstanceOf[js.Dynamic]
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