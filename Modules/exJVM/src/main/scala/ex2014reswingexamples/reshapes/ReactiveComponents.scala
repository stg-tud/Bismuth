package ex2014reswingexamples.reshapes

import reactives.default.*

import scala.language.implicitConversions
import scala.swing.*
import scala.swing.event.{ButtonClicked, ValueChanged}

/** Minimal reactive wrappers around the Swing components used by the ReShapes
  * example. They wrap a bit of Swing in a reactive interface directly, mirroring
  * the style used by the other `ex201x` examples, instead of relying on the
  * `reswing` library.
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

/** A `Slider` whose value is exposed as a reactive signal. */
class ReactiveSlider(
    min: Int,
    max: Int,
    initValue: Int,
    minorTickSpacing: Int = 0,
    paintTicks: Boolean = false
) {
  val peer: Slider = new Slider
  peer.min = min
  peer.max = max
  peer.value = initValue
  peer.minorTickSpacing = minorTickSpacing
  peer.paintTicks = paintTicks

  private val valueVar: Var[Int] = Var(initValue)
  val value: Signal[Int]         = Signal { valueVar.value }

  private val reactor = new Reactor {
    reactions += { case ValueChanged(_) => valueVar.set(peer.value) }
  }
  reactor.listenTo(peer)
}


/** A `BoxPanel` whose children are bound to a reactive signal. */
class ReactiveBoxPanel(
    orientation: Orientation.Value,
    components: Signal[Seq[Component]] = Signal { Seq.empty[Component] }
) extends BoxPanel(orientation) {
  contents ++= components.now
  components.changed observe { comps =>
    contents.clear()
    contents ++= comps
    repaint()
  }
}

/** A `MenuItem` that exposes its clicks as a reactive event and whose enabled
  * state can be driven by a reactive signal. */
@scala.annotation.nowarn("msg=shadows field")
class ReactiveMenuItem(
    text: String,
    enabled: Signal[Boolean] = Signal { true }
) extends MenuItem(text) with Reactor {
  private val clickedEvt: Evt[ButtonClicked] = Evt[ButtonClicked]()
  val clicked: Event[ButtonClicked]          = clickedEvt
  listenTo(this)
  reactions += { case c @ ButtonClicked(_) => clickedEvt.fire(c) }

  this.enabled = enabled.now
  enabled.changed observe { e => this.enabled = e }
}

/** A `Menu` whose text and child items can be driven by reactive signals. */
@scala.annotation.nowarn("msg=shadows field")
class ReactiveMenu(
    text: String,
    items: Signal[Seq[Component]] = Signal { Seq.empty[Component] }
) extends Menu(text) with Reactor {
  contents ++= items.now
  items.changed observe { comps =>
    contents.clear()
    contents ++= comps
  }
}
