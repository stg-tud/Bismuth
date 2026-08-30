package ex2014reswingexamples.dropdown

import reactives.default.*

import javax.swing.{DefaultComboBoxModel, JComboBox}
import scala.language.implicitConversions
import scala.swing.event.SelectionChanged
import scala.swing.{ComboBox, Reactor}

/** A combo box whose list of options is driven by a reactive signal and whose
  * current selection is exposed as a reactive signal as well.
  *
  * This is the direct Swing + reactives replacement for the old
  * `ReDynamicComboBox` that used to be built on the `reswing` library.
  */
class ReDynamicComboBox[A](
    options: Signal[List[A]] = Signal { List.empty[A] },
    initialSelection: Int = -1
) {
  val peer: ComboBox[A] = new ComboBox[A](Nil: List[A])
  private val peerBox: JComboBox[A] = peer.peer.asInstanceOf[JComboBox[A]]

  private val selectionVar: Var[Int] = Var(initialSelection)

  /** The index of the currently selected element, as a reactive signal. */
  val selection: Signal[Int] = Signal { selectionVar.value }

  setChoices(options.now, initialSelection)

  // Keep the model in sync with the options signal.
  options.changed observe { opts =>
    val current = peerBox.getSelectedIndex
    setChoices(opts, if current >= 0 && current < opts.size then current else -1)
  }

  // Reflect user selections back into the reactive signal.
  private val reactor = new Reactor {
    reactions += { case SelectionChanged(_) => selectionVar.set(peerBox.getSelectedIndex) }
  }
  reactor.listenTo(peer.selection)

  private def setChoices(opts: List[A], select: Int): Unit = {
    val model = new DefaultComboBoxModel[A]()
    opts.foreach(model.addElement)
    peerBox.setModel(model)
    peerBox.setSelectedIndex(select)
    selectionVar.set(select)
  }
}
