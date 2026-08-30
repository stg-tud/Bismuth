package ex201x.reswingexamples.dropdown

import reactives.default.*

import scala.swing.{BoxPanel, FlowPanel, Frame, Label, MainFrame, Orientation, SimpleSwingApplication}

object DropdownSample0 extends SimpleSwingApplication {

  val inputField                            = new ReactiveTextField("Berlin, Paris, London, Rome", 50)
  val inputText: Signal[String]             = Signal { inputField.text_out.value }
  val commaSeparated: Signal[List[String]]  =
    Signal { if inputText.value == null then Nil else inputText.value.split(",\\s*").toList }

  val dropdown                            = new ReDynamicComboBox(options = commaSeparated, initialSelection = -1)
  val selectionIndex: Signal[Int]         = Signal { dropdown.selection.value }
  val validSelection: Signal[Option[Int]] =
    Signal {
      if commaSeparated.value.indices.contains(selectionIndex.value) then Some(selectionIndex.value) else None
    }

  // select the currently selected item manually
  val currentSelectedItem: Signal[Option[String]] = Signal { validSelection.value.map(i => commaSeparated.value(i)) }
  val outputString: Signal[String]                = Signal { currentSelectedItem.value.getOrElse("Nothing") }
  val outputField                                 = new ReactiveTextField("Nothing", 20)
  outputField.text = outputString

  /* Debug output */
  // commaSeparated.changed observe { a => println(a) }
  // validSelection.changed observe { a => println(a)}
  // outputString.changed observe { a => println(a)}

  def top: Frame =
    new MainFrame {
      title = "Dropdown example 0"
      contents = new BoxPanel(Orientation.Vertical) {

        contents += new FlowPanel {
          contents += new Label("Comma-separated values: ")
          contents += inputField
        }
        contents += new FlowPanel {
          contents += new Label("Dropdown selection: ")
          contents += dropdown.peer
        }

        contents += new FlowPanel {
          contents += new Label("Selected item: ")
          contents += outputField
        }
      }
    }
}
