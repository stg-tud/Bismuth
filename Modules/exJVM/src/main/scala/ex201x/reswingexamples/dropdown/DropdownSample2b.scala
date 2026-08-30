package ex201x.reswingexamples.dropdown

import reactives.default.*

import scala.swing.{BoxPanel, FlowPanel, Frame, Label, MainFrame, Orientation, SimpleSwingApplication}

object DropdownSample2b extends SimpleSwingApplication {

  def top: Frame =
    new MainFrame {

      /* This version "artificially" introduces a Signal[List[Signal[String]] to illustrate higher
       * order signals, but this is not really necessary (See DropdownSample1)
       */

      val col1 = new ReactiveTextField("Berlin", 30)
      val col2 = new ReactiveTextField("Paris", 30)
      val col3 = new ReactiveTextField("London", 30)
      val col4 = new ReactiveTextField("Rome", 30)

      val val1: Signal[String] = Signal { col1.text_out.value }
      val val2: Signal[String] = Signal { col2.text_out.value }
      val val3: Signal[String] = Signal { col3.text_out.value }
      val val4: Signal[String] = Signal { col4.text_out.value }

      val listOfSignals: Signal[List[Signal[String]]] = Signal { List(val1, val2, val3, val4) }
      val options                                     = listOfSignals.flatten

      val innerChanged: Signal[List[Event[String]]] = Signal { listOfSignals.value.map(_.changed) }
      val anyChangedWrapped: Signal[Event[String]]  = Signal { innerChanged.value.reduce((a, b) => a || b) }
      val anyChanged                                = anyChangedWrapped.flatten

      anyChanged observe { x => println("some value has changed: " + x) }

      val dropdown                            = new ReDynamicComboBox(options = options, initialSelection = -1)
      val selectionIndex: Signal[Int]         = Signal { dropdown.selection.value }
      val validSelection: Signal[Option[Int]] =
        Signal { if options.value.indices.contains(selectionIndex.value) then Some(selectionIndex.value) else None }

      // select the currently selected item manually
      val currentSelectedItem: Signal[Option[String]] =
        Signal.dynamic { validSelection.value.map { i => listOfSignals.value(i).value } }
      val outputString: Signal[String] = Signal { currentSelectedItem.value.getOrElse("Nothing") }
      val outputField                  = new ReactiveTextField("Nothing", 30)
      outputField.text = outputString

      title = "Dropdown example 2b"
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
          contents += dropdown.peer
        }

        contents += new FlowPanel {
          contents += new Label("Selected item: ")
          contents += outputField
        }
      }
    }
}
