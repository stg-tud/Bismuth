package ex201x.reswingexamples.dropdown

import ex2013reswing.ReTextField
import reactives.default.*

import scala.swing.{BoxPanel, FlowPanel, Label, MainFrame, Orientation, SimpleSwingApplication}
import scala.swing.Frame

object DropdownSample1 extends SimpleSwingApplication {

  def top: Frame = {
    new MainFrame {

      val col1 = new ReTextField(text = "Berlin", columns = 30)
      val col2 = new ReTextField(text = "Paris", columns = 30)
      val col3 = new ReTextField(text = "London", columns = 30)
      val col4 = new ReTextField(text = "Rome", columns = 30)

      val val1: Signal[String] = Signal { col1.text.value }
      val val2: Signal[String] = Signal { col2.text.value }
      val val3: Signal[String] = Signal { col3.text.value }
      val val4: Signal[String] = Signal { col4.text.value }

      val listOfValues: Signal[List[String]] = Signal { List(val1.value, val2.value, val3.value, val4.value) }

      val options: Signal[List[String]] =
        Signal {
          listOfValues.value.filter { x =>
            if x != null then
              x.nonEmpty
            else
              false
          }
        }

      // val listOfSignals = Signal {List(val1, val2, val3, val4)}

      val dropdown       = new ReDynamicComboBox(options = options, selection = -1)
      val selectionIndex: Signal[Int] = Signal { dropdown.selection.value }
      val validSelection: Signal[Option[Int]] =
        Signal { if options.value.indices.contains(selectionIndex.value) then Some(selectionIndex.value) else None }

      // select the currently selected item manually
      val currentSelectedItem: Signal[Option[String]] = Signal { validSelection.value.map(i => options.value(i)) }
      val outputString: Signal[String]        = Signal { currentSelectedItem.value.getOrElse("Nothing") }
      val outputField         = new ReTextField(text = outputString)

      title = "Dropdown example 1"
      contents = new BoxPanel(Orientation.Vertical) {

        contents += new FlowPanel {
          contents += new Label("Value 1:")
          contents += col1
        }

        contents += new FlowPanel {
          contents += new Label("Value 2:")
          contents += col2
        }

        contents += new FlowPanel {
          contents += new Label("Value 3:")
          contents += col3
        }

        contents += new FlowPanel {
          contents += new Label("Value 4:")
          contents += col4
        }

        contents += new FlowPanel {
          contents += new Label("Dropdown selection: ")
          contents += dropdown
        }

        contents += new FlowPanel {
          contents += new Label("Selected item: ")
          contents += outputField
        }
      }
    }
  }
}
