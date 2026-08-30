package ex201x.reswingexamples.reshapes.ui.panels

import ex201x.reswingexamples.reshapes.{ReactiveBoxPanel, ReactiveButton, ReShapes}
import ex201x.reswingexamples.reshapes.drawing.{DeleteShape, DrawingSpaceState}
import ex201x.reswingexamples.reshapes.figures.Shape
import ex201x.reswingexamples.reshapes.util.ReactiveUtil.UnionEvent
import reactives.default.*

import java.awt.Color
import scala.swing.*
import scala.swing.event.MouseClicked

/** Lists all drawn shapes */
class ShapePanel extends BoxPanel(Orientation.Vertical) {
  def state = ReShapes.drawingSpaceState

  val shapes: Signal[List[Shape]] =
    Signal.dynamic { if state.value != null then state.value.shapes.value else List.empty } // #SIG

  val shapeViews: Signal[List[ShapeView]] =
    Signal { shapes.value map { shape => new ShapeView(shape, state.value) } } // #SIG

  val shapesPanel = new ReactiveBoxPanel(
    Orientation.Vertical,
    Signal[Seq[Component]] { // #SIG
      shapeViews.value map { (shapeView: ShapeView) => shapeView: Component }
    }
  )

  contents += new ScrollPane {
    contents = shapesPanel
  }

  val deleted: Event[DeleteShape] =
    UnionEvent(Signal { shapeViews.value map { shapeView => shapeView.deleted } }) // #SIG //#UE( //#EVT //#IF )
}

class ShapeView(shape: Shape, state: DrawingSpaceState) extends BoxPanel(Orientation.Horizontal) with Reactor {
  val SELECTED_COLOR     = new Color(0, 153, 255)
  val NOT_SELECTED_COLOR = new Color(255, 255, 255)

  val deleteButton = new ReactiveButton("delete")

  val deleted: Event[DeleteShape] = // #EVT
    deleteButton.clicked map { (_: Any) => new DeleteShape(shape) } // #EF

  background = NOT_SELECTED_COLOR
  contents += new Label(shape.toString)
  contents += deleteButton

  listenTo(mouse.clicks)
  reactions += {
    case MouseClicked(_, _, _, _, _) =>
      state.select.fire(if state.selectedShape.now != shape then shape else null)
  }

  state.selectedShape.changed observe { selected => // #HDL
    background = if selected == shape then SELECTED_COLOR else NOT_SELECTED_COLOR
  }
}