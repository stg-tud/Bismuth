package ex201x.programming2016demo.ui

import reactives.default.*

import java.awt.event.*
import java.awt.{Event as _, Shape as _, *}
import java.util.NoSuchElementException
import scala.swing.Panel

case class Point(x: Int, y: Int)

class ShapesPanel(val shapes: Signal[Iterable[Shape]]) extends Panel {
  // val allChanges: Event[Any] = Event { shapes().find{ shape: Shape => shape.changed().isDefined } }
  val allChanges: Event[Any] = shapes.map(_.map(_.changed)).flatten[Event[Any]](using Flatten.firstFiringEvent)

  allChanges observe { _ => repaint() }

  override def paintComponent(g: Graphics2D): Unit = {
    reactives.SelectedScheduler.candidate.scheduler.forceNewTransaction() { implicit turn =>
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, size.width, size.height)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.translate(size.width / 2, size.height / 2)
      for shape <- turn.now(shapes) do {
        try
          shape.drawSnapshot(g)
        catch {
          case _: NoSuchElementException => // ignore
          case _: IllegalStateException  => // ignore
        }
      }
    }
  }

  val _size: Var[Dimension]      = Var(size)
  val sigSize: Signal[Dimension] = _size
  peer.addComponentListener(new ComponentListener {
    override def componentShown(e: ComponentEvent): Unit   = {}
    override def componentHidden(e: ComponentEvent): Unit  = {}
    override def componentMoved(e: ComponentEvent): Unit   = {}
    override def componentResized(e: ComponentEvent): Unit = _size.set(size)
  })

  val width: Signal[Int]  = _size.map(_.width)
  val height: Signal[Int] = _size.map(_.height)

  object Mouse {
    class MouseButton {
      val pressed: Evt[Point]    = Evt[Point]()
      val released: Evt[Point]   = Evt[Point]()
      val clicked: Evt[Point]    = Evt[Point]()
      val state: Signal[Boolean] = (pressed.map(_ => true) || released.map(_ => false)).hold(false)
    }
    val _position: Var[Point]        = Var[Point](Point(0, 0))
    val x: Signal[Int]               = _position.map(_.x)
    val y: Signal[Int]               = _position.map(_.y)
    val wheel: Evt[Int]              = Evt[Int]()
    val _buttons: Array[MouseButton] = (0 until MouseInfo.getNumberOfButtons).map { _ => new MouseButton }.toArray

    def button(id: Int): MouseButton = _buttons(id - 1)
    val leftButton: MouseButton      = button(1)
    val middleButton: MouseButton    = button(2)
    val rightButton: MouseButton     = button(3)

    def translatePoint(from: java.awt.Point): Point =
      Point(from.x - size.width / 2, from.y - size.height / 2)
    val listener: MouseAdapter = new MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit  = button(e.getButton()).pressed.fire(translatePoint(e.getPoint))
      override def mouseReleased(e: MouseEvent): Unit = button(e.getButton()).released.fire(translatePoint(e.getPoint))

      override def mouseMoved(e: MouseEvent): Unit   = _position.set(translatePoint(e.getPoint))
      override def mouseDragged(e: MouseEvent): Unit = _position.set(translatePoint(e.getPoint))

      override def mouseWheelMoved(e: MouseWheelEvent): Unit = wheel.fire(e.getScrollAmount)
    }
    peer.addMouseListener(listener)
    peer.addMouseMotionListener(listener)
    peer.addMouseWheelListener(listener)
  }

  object Keyboard {
    val pressed: Evt[KeyEvent]  = Evt[KeyEvent]()
    val typed: Evt[KeyEvent]    = Evt[KeyEvent]()
    val released: Evt[KeyEvent] = Evt[KeyEvent]()
    val listener: KeyListener   = new KeyListener {
      override def keyPressed(e: KeyEvent): Unit  = pressed.fire(e)
      override def keyTyped(e: KeyEvent): Unit    = typed.fire(e)
      override def keyReleased(e: KeyEvent): Unit = released.fire(e)
    }

    peer.setFocusable(true)
    peer.addKeyListener(listener)

  }
}
