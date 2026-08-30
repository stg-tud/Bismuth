package ex2014reswingexamples.millgame.versions.signals

import reactives.default.*

import scala.swing.*
import scala.swing.event.{MouseDragged, MouseMoved, MouseReleased, UIElementMoved, UIElementResized}

/** A `Label` whose text is bound to a reactive signal, with optional size/font. */
@scala.annotation.nowarn("msg=shadows field")
class ReactiveLabel(
    text: Signal[String] = Signal { "" },
    preferredSize: Dimension = null,
    font: java.awt.Font = null
) extends Label(text.now) {
  text.changed observe { s => this.text = s }
  if preferredSize != null then this.preferredSize = preferredSize
  if font != null then this.font = font
}

/** A `Component` that exposes its size and bounds, and its mouse events, as
  * reactive events/signals.
  *
  * Direct Swing + reactives replacement for the old `ReComponent` from the
  * `reswing` library.
  */
class ReactiveComponent extends Component with Reactor {
  private val sizeVar   = Var(size)
  private val boundsVar = Var(bounds)

  val sizeS: Signal[Dimension]   = sizeVar
  val boundsS: Signal[Rectangle] = boundsVar

  private val mouseMovedEvt    = Evt[MouseMoved]()
  private val mouseDraggedEvt  = Evt[MouseDragged]()
  private val mouseReleasedEvt = Evt[MouseReleased]()

  val mouseMoved: Event[MouseMoved]    = mouseMovedEvt
  val mouseDragged: Event[MouseDragged] = mouseDraggedEvt
  val mouseReleased: Event[MouseReleased] = mouseReleasedEvt

  listenTo(this, mouse.moves, mouse.clicks)
  reactions += {
    case _: UIElementResized =>
      sizeVar.set(size); boundsVar.set(bounds)
    case _: UIElementMoved =>
      boundsVar.set(bounds)
    case e: MouseMoved =>
      mouseMovedEvt.fire(e)
    case e: MouseDragged =>
      mouseDraggedEvt.fire(e)
    case e: MouseReleased =>
      mouseReleasedEvt.fire(e)
  }
}
