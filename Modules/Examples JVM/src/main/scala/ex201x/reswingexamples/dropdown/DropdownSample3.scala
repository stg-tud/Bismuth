package ex201x.reswingexamples.dropdown

import ex2013reswing.ReTextField
import reactives.default.*

import scala.swing.*

object DropdownSample3 extends SimpleSwingApplication {

  // initial values
  val col1 = new ReTextField(text = "Berlin", columns = 30)
  val col2 = new ReTextField(text = "Paris", columns = 30)
  val val1: Signal[String] = Signal { col1.text.value }
  val val2: Signal[String] = Signal { col2.text.value }

  val fields: Var[List[Signal[String]]] = Var(List(val1, val2))
  val nFields: Signal[Int]                           = Signal { fields.value.size }

  val listOfSignals: Signal[List[Signal[String]]] = Signal { fields.value }
  val options       = listOfSignals.flatten

  val innerChanged: Signal[List[Event[String]]]      = Signal { listOfSignals.value.map(_.changed) }
  val anyChangedWrapped: Signal[Event[String]] = Signal { innerChanged.value.reduce((a, b) => a || b) }
  val anyChanged        = anyChangedWrapped.flatten

  anyChanged observe { x => println("some value has changed: " + x) }

  val dropdown       = new ReDynamicComboBox(options = options, selection = -1)
  val selectionIndex: Signal[Int] = Signal { dropdown.selection.value }
  val validSelection: Signal[Option[Int]] =
    Signal { if options.value.indices.contains(selectionIndex.value) then Some(selectionIndex.value) else None }

  // select the currently selected item manually
  val currentSelectedItem: Signal[Option[String]] = Signal.dynamic { validSelection.value.map { i => listOfSignals.value(i).value } }
  val outputString: Signal[String]        = Signal { currentSelectedItem.value.getOrElse("Nothing") }
  val outputField         = new ReTextField(text = outputString)

  object frame extends MainFrame {
    title = "Dropdown example 3"
    val fields: BoxPanel = new BoxPanel(Orientation.Vertical) {

      contents += new FlowPanel {
        contents += new Label("Dropdown selection: ")
        contents += dropdown
      }

      contents += new FlowPanel {
        contents += new Label("Selected item: ")
        contents += outputField
      }

      contents += new FlowPanel {
        contents += Button("Add value")(addField("empty"))
      }

      contents += new FlowPanel {
        contents += new Label("Value 1:")
        contents += col1
      }

      contents += new FlowPanel {
        contents += new Label("Value 2:")
        contents += col2
      }

    }
    contents = fields
  }

  def top = frame

  def addField(initText: String): Unit = {
    val n   = nFields.readValueOnce + 1
    val col = new ReTextField(text = initText, columns = 30)
    frame.fields.contents += new FlowPanel {
      contents += new Label("Value " + n + ":")
      contents += col
    }
    val content: Signal[String] = Signal { col.text.value }
    fields `set` content :: fields.readValueOnce
    frame.pack()
  }

}
