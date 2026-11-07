package ex2014swing.path

import java.awt.{Color, Graphics2D, Point, geom}
import scala.swing.Swing.*
import scala.swing.event.*
import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}

object LinePainting extends SimpleSwingApplication {

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

    panel.reactions += {
      case e: MousePressed =>
        panel.moveTo(e.point)
        panel.requestFocusInWindow()
        ()
      case e: MouseDragged        => panel.lineTo(e.point)
      case e: MouseReleased       => panel.lineTo(e.point)
      case KeyTyped(_, 'c', _, _) =>
        panel.path = new geom.GeneralPath
        panel.repaint()
      case _: FocusLost => panel.repaint()
    }
    panel
  }

  def top: Frame =
    new MainFrame {
      title = "Simple Line Painting Demo"
      contents = ui
    }
}
