package ex2014swing.path

import reactives.default.*

import java.awt.{Color, Graphics2D, Point, geom}
import scala.swing.Swing.*
import scala.swing.event.*
import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}

object LinePaintingEScala extends SimpleSwingApplication {

  /** records the dragging and paints it */
  class LinePanel extends Panel {
    var path = new geom.GeneralPath

    def lineTo(p: Point): Unit = { path.lineTo(p.x.toFloat, p.y.toFloat); repaint() }
    def moveTo(p: Point): Unit = { path.moveTo(p.x.toFloat, p.y.toFloat); repaint() }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.setColor(new Color(100, 100, 100))
      g.drawString(
        "Press left mouse button and drag to paint." +
        (if hasFocus then " Press 'c' to clear." else ""),
        10,
        size.height - 10
      )
      g.setColor(Color.black)
      g.draw(path)
    }
  }

  lazy val ui: Panel = {
    val panel = new LinePanel
    panel.background = Color.white
    panel.preferredSize = (1000, 500)
    panel.focusable = true
    panel.listenTo(panel.mouse.clicks, panel.mouse.moves, panel.keys)

    /* EScala events */
    val mousePressed: Evt[Point]  = Evt[Point]()
    val mouseDragged: Evt[Point]  = Evt[Point]()
    val mouseReleased: Evt[Point] = Evt[Point]()
    val cKeyTyped: Evt[Unit]      = Evt[Unit]()
    val focusLost: Evt[Unit]      = Evt[Unit]()
    /* Bind the EScala events to the Swing events */
    panel.reactions += {
      case e: MousePressed        => mousePressed.fire(e.point)
      case e: MouseDragged        => mouseDragged.fire(e.point)
      case e: MouseReleased       => mouseReleased.fire(e.point)
      case KeyTyped(_, 'c', _, _) => cKeyTyped.fire()
      case _: FocusLost           => focusLost.fire()
    }

    /* Attach handlers to the EScala events */
    mousePressed observe { p =>
      panel.moveTo(p); panel.requestFocusInWindow(); ()
    }
    mouseDragged observe { panel.lineTo(_) }
    mouseReleased observe { panel.lineTo(_) }
    cKeyTyped observe { _ =>
      panel.path = new geom.GeneralPath; panel.repaint()
    }
    focusLost observe { _ => panel.repaint() }
    panel
  }

  def top: Frame =
    new MainFrame {
      title = "Simple Line Painting Demo"
      contents = ui
    }
}
