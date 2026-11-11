package ex2014swing.bouncing

import reactives.default.*

import java.awt.{Dimension, Graphics2D, Point}
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object SwitchVersion extends SimpleSwingApplication {
  lazy val application = new SwitchVersion
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while true do {
      Swing onEDTWait application.tick.fire()
      Thread `sleep` 20
    }
  }
}

class SwitchVersion {
  val Size         = 50
  val Max_X        = 600
  val Max_Y        = 600
  val initPosition = new Point(20, 10)
  val speed        = new Point(10, 8)

  val tick: Evt[Unit] = Evt[Unit]()

  // Using switch: position and speed depend on each other, which local lazy vals allow
  // (unlike class fields, which would be accessed before they are initialized)
  private def bouncing(init: Int, speed: Int, max: Int): Signal[Int] = {
    lazy val pos: Signal[Int]    = tick.fold(init) { (pos, _) => pos + speedS.now }
    lazy val bounce: Event[Int]  = pos.changed && (p => p < 0 || p + Size > max)
    lazy val speedS: Signal[Int] = bounce.toggle(Var(speed), Var(-speed))
    pos
  }

  val x: Signal[Int] = bouncing(initPosition.x, speed.x, Max_X)
  val y: Signal[Int] = bouncing(initPosition.y, speed.y, Max_Y)

  // drawing code
  val canvas: Panel = new Panel() {
    override def paintComponent(g: Graphics2D): Unit =
      transaction(x, y) { t ?=>
        g.fillOval(t.now(x), t.now(y), Size, Size)
      }
  }
  canvas.preferredSize = new Dimension(600, 600)
  val frame: MainFrame = new MainFrame
  frame.contents = canvas

  tick observe { (_: Unit) => frame.repaint() }
}
