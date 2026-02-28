package webapps.calendar

import org.scalajs.dom.html.{Button, Div, Input, LI}
import reactives.default.*
import reactives.extra.Tags.reattach
import reactives.operator.Event

import scalatags.JsDom.all.*

case class Appointment(name: String, start: Int, end: Int) {
  def days: Int = end - start

  private val removeButton: (event: Event[Unit], data: Button) =
    Event.fromCallback(button("Remove", onclick := Event.handle).render)
  private val editButton: (event: Event[Unit], data: Button) =
    Event.fromCallback(button("Edit", onclick := Event.handle).render)

  private val nameInput: Input = input(value := name).render

  private val startInput: Input = input(value := start, `type` := "number").render
  private val endInput: Input   = input(value := end, `type` := "number").render

  private val commitChange: (event: Event[Unit], data: Button) =
    Event.fromCallback(button("Commit", onclick := Event.handle).render)
  private val currentlyEditing = (editButton.event || commitChange.event).fold(false)((s, _) => !s)

  lazy val removeEvent: Event[Appointment]              = removeButton.event.snap { this }
  lazy val editEvent: Event[(Appointment, Appointment)] = commitChange.event.snap {
    (this, Appointment(nameInput.value, startInput.value.toInt, endInput.value.toInt))
  }

  val tagSignal: Signal[LI] = currentlyEditing.map { c =>
    if !c then li(span(s"$name: $start -> $end").render, br().render, removeButton.data, editButton.data).render
    else li(nameInput, span(": ").render, startInput, span(" -> ").render, endInput, commitChange.data).render
  }

  def toTag: Div =
    div().render.reattach(tagSignal)
}

type Calendar = Set[Appointment]
