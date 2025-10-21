package ex201x.swing.smashingparticles

import reactives.default.*

import java.awt.{Dimension, Graphics2D, Point}
import scala.collection.mutable.ListBuffer
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object SmashingParticles extends SimpleSwingApplication {
  lazy val application = new SmashingParticles
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while true do {
      Swing onEDTWait {
        application.base.transform(_ + 1)
        application.frame.repaint()
      }
      Thread `sleep` 20
    }
  }
}

class SmashingParticles {

  val toDraw: ListBuffer[Graphics2D => Unit] = ListBuffer[Function1[Graphics2D, Unit]]()
  type Delta = Point

  class Oval(center: Signal[Point], radius: Signal[Int]) {
    toDraw += ((g: Graphics2D) => g.fillOval(center.now.x, center.now.y, radius.now, radius.now))

    override def toString: String = "Circle(" + center + "," + radius + ")"
  }

  val base: Var[Int]       = Var(0)
  val simpleTime: Signal[Int] = Signal { base.value }
  val time: Signal[Int]       = Signal { simpleTime.value % 200 } // cyclic time :)

  val point1: Signal[Point] = Signal { new Point(20 + time.value, 20 + time.value) }
  new Oval(point1, time)
  val point2: Signal[Point] = Signal { new Point(40 + time.value, 80 + time.value) }
  new Oval(point2, time)
  val point3: Signal[Point] = Signal { new Point(80 + time.value, 40 + time.value) }
  new Oval(point3, time)
  val point4: Signal[Point] = Signal { new Point(160 + time.value, 160 + time.value) }
  new Oval(point4, time)

  // drawing code
  def top   = frame
  val frame: MainFrame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D): Unit =
        toDraw.foreach(x => x(g))
    }
  }

}
