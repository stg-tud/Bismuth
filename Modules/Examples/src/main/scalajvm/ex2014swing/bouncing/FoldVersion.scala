package ex2014swing.bouncing

import reactives.default.*

import java.awt.{Dimension, Graphics2D, Point}
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object FoldVersion extends SimpleSwingApplication {
  lazy val application = new FoldVersion
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while true do {
      Swing onEDTWait application.tick.fire()
      Thread `sleep` 20
    }
  }
}

class FoldVersion {
  val Size         = 50
  val Max_X        = 600
  val Max_Y        = 600
  val initPosition = new Point(20, 10)
  val speed        = new Point(10, 8)

  val tick: Evt[Unit] = Evt[Unit]()
  // Implementing switch with fold: the direction switches whenever the position leaves the bounds
  def bounce(speed: Int, max: Int)(state: (Int, Boolean)): (Int, Boolean) = {
    val (pos, switched) = state
    val next            = pos + (if switched then speed else -speed)
    (next, if next < 0 || next + Size > max then !switched else switched)
  }

  val xState: Signal[(Int, Boolean)] = tick.fold((initPosition.x, false)) { (state, _) => bounce(speed.x, Max_X)(state) }
  val yState: Signal[(Int, Boolean)] = tick.fold((initPosition.y, false)) { (state, _) => bounce(speed.y, Max_Y)(state) }

  val xx: Signal[Int] = xState.map(_._1)
  val yy: Signal[Int] = yState.map(_._1)

  // drawing code
  val canvas: Panel = new Panel() {
    override def paintComponent(g: Graphics2D): Unit =
      transaction(xx, yy) { t ?=>
        g.fillOval(t.now(xx), t.now(yy), Size, Size)
      }
  }
  canvas.preferredSize = new Dimension(600, 600)
  val frame: MainFrame = new MainFrame
  frame.contents = canvas

  tick observe { (_: Unit) => frame.repaint() }
}
