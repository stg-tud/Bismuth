package ex201x.swing.clickcounter

import scala.annotation.unused
import scala.swing.*
import scala.swing.event.*

object ObserverSwingApp extends SimpleSwingApplication {
  def top: Frame =
    new MainFrame {

      /* Create the graphics */
      title = "Reactive Swing App"
      val button: Button = new Button {
        text = "Click me"
      }
      val label: Label = new Label {
        text = "No button clicks registered"
      }
      contents = new BoxPanel(Orientation.Vertical) {
        contents += button
        contents += label
        border = Swing.EmptyBorder(30, 30, 10, 30)
      }

      /* The logic */
      listenTo(button)
      @unused var nClicks = 0
      reactions += {
        case ButtonClicked(b) =>
          nClicks += 1
          label.text = "Number of button clicks: " + nClicks
          if nClicks > 0 then
              button.text = "Click me again"
      }
    }
}
