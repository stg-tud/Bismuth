package ex201x.reswingexamples.dropdown

import reactives.default.*

import scala.swing.*
import scala.swing.event.*

/** Minimal reactive wrapper around a Swing text component.
  *
  * Mirrors the style used by the other `ex201x` examples (see for instance the
  * `ex201x.swing` packages): we wrap a bit of Swing in a reactive interface
  * instead of relying on an external reactive-swing library.
  *
  *  - `field.text = "some string"` sets the text imperatively.
  *  - `field.text = someSignal` keeps the text in sync with a signal.
  *  - `field.text_out` exposes the current text as a signal.
  */
trait ReactiveText extends Reactor {
  protected lazy val textVar: Var[String] = Var(text)

  def text: String
  def text_=(s: String): Unit

  /** Keep the field's text in sync with the given signal. */
  def text_=(value: Signal[String]): Unit = {
    text = value.now
    value.changed observe { s => if text != s then text = s }
    ()
  }

  /** The current field text, as a reactive signal. */
  lazy val text_out: Signal[String] = Signal { textVar.value }
}

@scala.annotation.nowarn("msg=shadows field")
class ReactiveTextField(text: String, columns: Int) extends TextField(text, columns) with ReactiveText {
  listenTo(this)
  reactions += { case EditDone(_) => textVar.set(text) }
}