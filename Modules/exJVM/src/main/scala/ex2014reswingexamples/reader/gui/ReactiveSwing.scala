package ex2014reswingexamples.reader.gui

import reactives.default.*

import scala.swing.*
import scala.swing.event.*

/** A `ListView` whose content is bound to a reactive signal and that exposes its
  * current selection as a reactive signal.
  *
  * This is a direct Swing + reactives replacement for the old `ReListView` and
  * `ReListViewEx` classes that used to be built on the `reswing` library.
  */
class ReListViewEx[A](visibleRows: Int) extends ListView[A] with Reactor {
  visibleRowCount = visibleRows

  private val listDataVar: Var[Seq[A]] = Var(Seq.empty[A])

  /** Bind the list contents to the given reactive signal. */
  def bind(listData: Signal[Seq[A]]): Unit = {
    listDataVar.set(listData.now)
    listData.changed observe { seq => listDataVar.set(seq) }
    (): Unit
  }

  // Push list data changes into the underlying JList model.
  listDataVar.changed observe { seq =>
    val model = new javax.swing.DefaultListModel[A]()
    seq.foreach(model.addElement)
    peer.setModel(model)
    ()
  }

  private val selectionVar: Var[Int] = Var(-1)

  listenTo(selection)
  reactions += { case ListSelectionChanged(_, _, _) => selectionVar.set(peer.getSelectedIndex) }

  /** Index of the currently selected row, as a signal. */
  val selectedIndex: Signal[Int] = Signal { selectionVar.value }

  /** The currently selected element, as a signal. */
  val selectedItem: Signal[Option[A]] = Signal {
    val i = selectionVar.value
    if i >= 0 && i < listDataVar.value.size then Some(listDataVar.value(i)) else None
  }
}
