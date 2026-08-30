package ex201x.reswingexamples.reshapes.ui.panels

import ex201x.reswingexamples.reshapes.{ReactiveButton, ReactiveSlider}
import reactives.default.*

import java.awt.Color
import javax.swing.JColorChooser
import scala.swing.{Action, BoxPanel, Button, Component, FlowPanel, Frame, Label, Orientation}

/** Panel for various customization of the stroke. */
class StrokeInputPanel extends FlowPanel {
  private val colorWindow = new ColorWindow

  private val slider = new ReactiveSlider(min = 1, max = 50, initValue = 1, minorTickSpacing = 1, paintTicks = true)

  private val showColorWindow = new ReactiveButton("Show Colorinput")
  showColorWindow.clicked observe { _ => colorWindow.visible = !colorWindow.visible }

  contents += new Label { text = "stroke width: " }
  contents += slider
  contents += showColorWindow

  val strokeWidth = slider.value
  val color       = colorWindow.color
}

class ColorWindow extends Frame {
  title = "Choose color"

  private object colorChooser extends Component {
    override lazy val peer: JColorChooser = new JColorChooser
  }

  contents = new BoxPanel(Orientation.Vertical) {
    contents += colorChooser
    contents += new Button(Action("OK") {
      color `set` colorChooser.peer.getColor
      ColorWindow.this.visible = false
    })
  }

  val color: Var[Color] = Var(Color.BLACK) // #VAR
}