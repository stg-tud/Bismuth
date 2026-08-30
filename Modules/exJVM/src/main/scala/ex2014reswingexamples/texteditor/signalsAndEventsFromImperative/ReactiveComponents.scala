package ex2014reswingexamples.texteditor.signalsAndEventsFromImperative

import reactives.default.*

import scala.swing.*
import scala.swing.event.{ButtonClicked, CaretUpdate, ValueChanged}

/** Minimal reactive wrappers around the Swing components used by the texteditor
  * example. They wrap a bit of Swing in a reactive interface directly, instead
  * of relying on the `reswing` library.
  */

/** A `Button` that exposes its clicks as a reactive event. */
@scala.annotation.nowarn("msg=shadows field")
class ReactiveButton(text: String) extends Button(text) with Reactor {
  private val clickedEvt: Evt[ButtonClicked] = Evt[ButtonClicked]()
  val clicked: Event[ButtonClicked]          = clickedEvt
  listenTo(this)
  reactions += { case c @ ButtonClicked(_) => clickedEvt.fire(c) }
}

/** A `Label` whose text is bound to a reactive signal. */
@scala.annotation.nowarn("msg=shadows field")
class ReactiveLabel(text: Signal[String]) extends Label(text.now) {
  text.changed observe { s => this.text = s }
}

/** A `scala.swing.TextArea` that exposes its document state as reactive signals.
  *
  * This replaces the old `ReTextArea` (backed by the `reswing` library) with a
  * plain Swing text area whose text, caret position and line count are exposed
  * as reactive signals.
  */
class ReactiveTextArea(text0: String) extends scala.swing.TextArea(text0) with Reactor {
  private val textVar        = Var(text)
  private val caretVar       = Var(caret.position)
  private val lineCountVar   = Var(lineCount)

  listenTo(this, caret)
  reactions += {
    case _: ValueChanged  => textVar.set(text); lineCountVar.set(lineCount)
    case _: CaretUpdate   => caretVar.set(caret.position)
  }

  val textSignal: Signal[String]     = textVar
  val caretPosSignal: Signal[Int]    = caretVar
  val lineCountSignal: Signal[Int]   = lineCountVar
}
