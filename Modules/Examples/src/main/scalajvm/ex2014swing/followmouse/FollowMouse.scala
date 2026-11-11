package ex2014swing.followmouse

import ex2014swing.Mouse
import reactives.default.*

import java.awt.{Dimension, Graphics2D}
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object FollowMouse extends SimpleSwingApplication {
  lazy val application = new FollowMouse
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while true do {
      Swing onEDTWait application.tick.fire()
      Thread `sleep` 10
    }
  }
}

class FollowMouse {

  val Max_X = 700
  val Max_Y = 600
  val Size  = 20
  val Range = 100

  // The whole logic

  val tick: Evt[Unit]      = Evt[Unit]()
  val time: Signal[Double] = tick.iterate(0.0) { (acc: Double) => (acc + 0.1) % (math.Pi * 2) }

  val mouse               = new Mouse
  val mouseX: Signal[Int] = Signal { mouse.position.value.getX.toInt - Size / 2 }
  val mouseY: Signal[Int] = Signal { mouse.position.value.getY.toInt - Size / 2 }

  val xOffset: Signal[Double] = Signal { math.sin(time.value) * Range }
  val yOffset: Signal[Double] = Signal { math.cos(time.value) * Range }

  val x: Signal[Int] = Signal { mouseX.value + xOffset.value.toInt }
  val y: Signal[Int] = Signal { mouseY.value + yOffset.value.toInt }

  // redraw code
  val stateChanged: Event[Any] = mouse.position.changed.||[Any](tick)

  // drawing code
  def top = frame
  // lazy, so the Swing components are wired up after construction
  lazy val canvas: Panel = {
    val canvas = new Panel() {
      // val scoreFont = new Font("Tahoma", java.awt.Font.PLAIN, 32)
      override def paintComponent(g: Graphics2D): Unit = {
        g.setColor(java.awt.Color.DARK_GRAY)
        g.fillOval(x.now, y.now, Size, Size)
      }
    }
    canvas.preferredSize = new Dimension(Max_X, Max_Y)
    canvas.listenTo(canvas.mouse.moves, canvas.mouse.clicks)
    canvas.reactions += mouse.react
    canvas
  }
  lazy val frame: MainFrame = {
    val frame = new MainFrame
    frame.title = "Rotating around the mouse"
    frame.resizable = false
    frame.contents = canvas
    frame
  }

  // redraw code
  stateChanged observe { _ => frame.repaint() }
}
