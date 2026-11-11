package webapps.ex2026minisocial

import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Button, Div, Input}
import rdts.base.{Lattice, ReplicaId}
import rdts.syntax.DeltaBuffer
import reactives.default.*
import reactives.extra.Tags.reattach
import reactives.operator.Fold.Branch
import reactives.operator.FoldState
import scalatags.JsDom.all.*

object MiniSocialUI {

  /** A `given` allows methods to find this replica id by its type if it is in scope. */
  given replicaId: ReplicaId = MiniSocialMain.replicaId

  /** helper function to remove some boilerplate in the Fold below */
  extension [T](event: Event[T])
      def deltaBranch[S: Lattice](f: FoldState[S] ?=> T => S): Fold.Branch[DeltaBuffer[S]] =
        event.branch { v => Fold.current.mod(app => f(using FoldState(app))(v)) }

  /** This resets the Delta buffer in the fold below, to not contain any deltas */
  def resetBuffer[T]: Branch[DeltaBuffer[T]] =
    Fold.Branch[DeltaBuffer[T]](Nil, isStatic = false, _ => Fold.current.clearDeltas())

  def makeInputEvent(placeholderText: String): (event: Event[String], data: Input) = {
    val handler = Event.fromCallback[Input, UIEvent](
      input(placeholder := placeholderText, oninput := Event.handle).render
    )
    val text = handler.event.map(_ => handler.data.value)
    (text, handler.data)
  }

  def makeButtonEvent(description: String): (event: Event[UIEvent], data: Button) =
    Event.fromCallback[Button, UIEvent](
      button(description, onclick := Event.handle).render
    )

  def getContents(): Div = {

    val (upvoteButtonEvent, upvoteButtonData)     = makeButtonEvent(Character.toString(0x1f44d))
    val (downvoteButtonEvent, downvoteButtonData) = makeButtonEvent(Character.toString(0x1f44e))
    val (messageEvent, messageField)              = makeInputEvent("Your message to the world …")
    upvoteButtonData.classList.add("vote")
    downvoteButtonData.classList.add("vote")

    val stateSignal: Signal[DeltaBuffer[MiniSocial]] =
      MiniSocialDataManager.hookup(MiniSocial()) { (init, incoming) =>
        Fold(init)(
          resetBuffer,

          messageEvent.deltaBranch { inputText =>
            Fold.current.setMessage(inputText)
          },
          upvoteButtonEvent.deltaBranch { _ =>
            Fold.current.like()
          },
          downvoteButtonEvent.deltaBranch { _ =>
            Fold.current.dislike()
          },
          incoming
        )
      }

    val appStateSignal = Signal { stateSignal.value.state }
    val messageSignal  = Signal {
      val message = appStateSignal.value.message.value
      if message.isEmpty then span(cls := "empty", "No message yet").render else span(message).render
    }
    val upvotesSignal   = Signal { span(cls := "count", appStateSignal.value.upvotes.value).render }
    val downvotesSignal = Signal { span(cls := "count", appStateSignal.value.downvotes.value).render }

    // the buttons show their count, the (static) emoji is the first child
    upvoteButtonData.appendChild(span().render.reattach(upvotesSignal))
    downvoteButtonData.appendChild(span().render.reattach(downvotesSignal))

    div(
      cls := "mini-social",
      div(cls := "message", p.render.reattach(messageSignal)),
      div(cls := "votes", upvoteButtonData, downvoteButtonData),
      label(cls := "compose", span("Change the message"), messageField),
    ).render

  }

}
