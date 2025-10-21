package ex201x

import reactives.default.*
import reactives.operator.Event

import java.awt.Point
import scala.swing.event.*

class Mouse {

  /* EScala events */
  val mouseMovedE: Evt[Point]    = Evt[Point]()
  val mousePressedE: Evt[Point]  = Evt[Point]()
  val mouseDraggedE: Evt[Point]  = Evt[Point]()
  val mouseReleasedE: Evt[Point] = Evt[Point]()
  /* Compose reactive values */
  val mouseChangePosition: Event[Point]      = mouseMovedE || mouseDraggedE
  val mousePressedOrReleased: Event[Point]   = mousePressedE || mouseReleasedE
  val position: Signal[Point]  = mouseChangePosition.hold(new Point(0, 0))
  val pressed: Signal[Boolean] =
    mousePressedOrReleased.toggle(Signal { false }, Signal { true }) // TODO: solve this more robust

  /* Scala swing reaction */
  val react: scala.swing.Reactions.Reaction = {
    case e: MouseMoved    => mouseMovedE.fire(e.point)
    case e: MousePressed  => mousePressedE.fire(e.point)
    case e: MouseDragged  => mouseDraggedE.fire(e.point)
    case e: MouseReleased => mouseReleasedE.fire(e.point)
  }

}
