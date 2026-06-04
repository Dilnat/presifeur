package presifeur.web

import presifeur.model.*
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, document, html}
import scala.scalajs.js

final class ThreeScene(container: html.Div):

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
