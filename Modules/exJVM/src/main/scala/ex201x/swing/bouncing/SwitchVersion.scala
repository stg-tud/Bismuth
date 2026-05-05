package ex201x.swing.bouncing

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
  // Using switch

  val x: Signal[Int] = tick.fold(initPosition.x) { (pos, _) => pos + speedX.now }
  val y: Signal[Int] = tick.fold(initPosition.y) { (pos, _) => pos + speedY.now }

  val xBounce: Event[Int] = x.changed && (x => x < 0 || x + Size > Max_X)
  val yBounce: Event[Int] = y.changed && (y => y < 0 || y + Size > Max_Y)

  val speedX: Signal[Int] = xBounce.toggle(Var(speed.x), Var(-speed.x))
  val speedY: Signal[Int] = yBounce.toggle(Var(speed.y), Var(-speed.y))

  tick observe { (_: Unit) => frame.repaint() }

  // drawing code
  val frame: MainFrame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D): Unit =
        transaction(x, y) { t ?=>
          g.fillOval(t.now(x), t.now(y), Size, Size)
        }
    }
  }
}
