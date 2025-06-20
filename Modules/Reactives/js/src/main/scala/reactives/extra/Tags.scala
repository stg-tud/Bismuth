package reactives.extra

import org.scalajs.dom
import org.scalajs.dom.console.log as println
import org.scalajs.dom.html.Input
import org.scalajs.dom.{Element, KeyboardEvent, Range, console, document}
import reactives.SelectedScheduler
import reactives.core.{CreationTicket, PlanTransactionScope}
import reactives.operator.*
import reactives.structure.RExceptions.ObservedException
import reactives.structure.{Observe, Pulse}

object Tags {

  trait RangeSplice[-A <: dom.Element, -T]:
    def splice(anchor: A, range: dom.Range, value: T): Unit
  object RangeSplice:
    given elem: RangeSplice[dom.Element, dom.Element] with {
      override def splice(anchor: dom.Element, range: Range, value: dom.Element) =
        range.insertNode(value)
    }
    given many[A <: dom.Element, T](using other: RangeSplice[A, T]): RangeSplice[A, Seq[T]] with {
      override def splice(anchor: A, range: Range, value: Seq[T]) =
        value.reverseIterator.foreach { v => other.splice(anchor, range, v) }
    }
    given string: RangeSplice[dom.Element, String] with {
      override def splice(anchor: dom.Element, range: Range, value: String) =
        anchor.textContent = value
    }

  extension [A <: dom.Element](anchor: A)
    def reattach[T](signal: Signal[T])(using
        splicer: RangeSplice[A, T],
        creationTicket: CreationTicket[SelectedScheduler.State]
    ): anchor.type = {
      val startMarker = document.createComment("reattach start")
      val endMarker   = document.createComment("reattach end")
      anchor.append(startMarker, endMarker)
      Observe.strong(signal, true) {
        tagObserver(anchor, signal) { v =>
          val range = document.createRange()
          range.setStartAfter(startMarker)
          range.setEndBefore(endMarker)
          range.deleteContents()
          splicer.splice(anchor, range, v)
        }
      }
      anchor
    }

  extension (input: Input)
    def inputEntered(using
        creationTicket: CreationTicket[SelectedScheduler.State],
        scheduler: PlanTransactionScope[SelectedScheduler.State]
    ): Event[String] = {
      val handler: Event.CBR[KeyboardEvent, Unit] = Event.fromCallback(input.onkeyup = Event.handle(_))

      handler.event
        .map { (e: KeyboardEvent) =>
          if e.key == "Enter" then
            val res = input.value.trim
            if res.nonEmpty then
              e.preventDefault()
              input.value = ""
              Some(res)
            else None
          else None
        }.flatten
    }

  /* This only returns true the second time it is called to prevent observers to directly trigger */
  def isInDocumentHack(elem: dom.Element): Any => Boolean = {
    var second = false
    _ => {
      if second then {
        !document.contains(elem)
      } else {
        second = true
        false
      }
    }
  }

  /** Tag observer removes the observer if it fires while the element is NOT in the dom. */
  def tagObserver[A](
      parent: dom.Element,
      rendered: Signal[A]
  )(fun: A => Unit)(reevalVal: Pulse[A]): Observe.ObserveInteract =
    new Observe.ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = {
        reevalVal match {
          case Pulse.empty(_) | Pulse.NoChange => false
          case Pulse.Exceptional(f)            =>
            throw ObservedException(rendered, s"signal tag attached to $parent observed", f)
          case Pulse.Value(v) =>
            isInDocumentHack(parent)(v)
        }
      }

      override def execute(): Unit =
        reevalVal match {
          case Pulse.empty(_) | Pulse.NoChange => ()
          case Pulse.Value(v)                  =>
            fun(v)
          case Pulse.Exceptional(f) =>
            throw new IllegalStateException("should have aborted earlier", f)
        }
    }

}
