package ex2014reswingexamples.reshapes.ui.panels

import ex2014reswingexamples.reshapes.{ReShapes, ReactiveBoxPanel, ReactiveButton}
import ex2014reswingexamples.reshapes.drawing.Command
import ex2014reswingexamples.reshapes.util.ReactiveUtil.UnionEvent
import reactives.default.*

import scala.swing.{BoxPanel, Component, Orientation, ScrollPane}

/** The CommandPanel lists all executed commands and makes it possible to revert them */
class CommandPanel extends BoxPanel(Orientation.Vertical) {
  def state = ReShapes.drawingSpaceState

  val commands: Signal[List[Command]] =
    Signal.dynamic { if state.value != null then state.value.commands.value else List.empty } // #SIG

  val buttonsEvents: Signal[List[(Component, Event[Command])]] = Signal { // #SIG
    commands.value map { command =>
      val button = new ReactiveButton(command.description) // #IS( //#EVT )
      (button: Component, button.clicked map { (_: Any) => command })
    }
  }

  val revert: Event[Command] = UnionEvent(Signal { // #SIG //#UE( //#EVT //#IF )
    buttonsEvents.value map { case (_, ev) => ev: Event[Command] }
  })

  val commandPanel = new ReactiveBoxPanel(
    Orientation.Vertical,
    Signal { (buttonsEvents.value map { case (btn, _) => btn }): Seq[Component] }
  ) // #SIG //#IS( // )

  contents += new ScrollPane {
    contents = commandPanel
  }
}
