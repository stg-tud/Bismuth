package webapps.ex2026minisocial

import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Button, Div, Input}
import rdts.base.{Lattice, LocalUid}
import rdts.syntax.DeltaBuffer
import reactives.default.*
import reactives.operator.FoldState
import reactives.extra.Tags.reattach
import scalatags.JsDom.all.*

object MiniSocialUI {

  /** A `given` allows methods to find this replica id by its type if it is in scope. */
  given replicaId: LocalUid = MiniSocialMain.replicaId

  /** helper function to remove some boilerplate in the Fold below */
  extension [T](event: Event[T])
    def deltaBranch[S: Lattice](f: FoldState[S] ?=> T => S): Fold.Branch[DeltaBuffer[S]] = {
      event.branch { v => Fold.current.mod(app => f(using FoldState(app))(v)) }
    }

  /** This resets the Delta buffer in the fold below, to not contain any deltas */
  def resetBuffer[T] = Fold.Branch[DeltaBuffer[T]](Nil, isStatic = false, _ => Fold.current.clearDeltas())

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

    val (upvoteButtonEvent, upvoteButtonData)   = makeButtonEvent(Character.toString(0x1f44d))
    val (downvoteButtonEvent, downvoteButtonData) = makeButtonEvent(Character.toString(0x1f44e))
    val (messageEvent, messageField)              = makeInputEvent("<your message to the world>")

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
    val messageSignal = Signal {
      span(appStateSignal.value.message.value).render
    }
    val upvotesSignal = Signal {
      span(appStateSignal.value.upvotes.value).render
    }
    val downvotesSignal = Signal {
      span(appStateSignal.value.downvotes.value).render
    }

    /* Just a DSL to create some HTML */
    div(
      table(
        thead(
          th("message to vote on"),
          th("upvotes"),
          th("downvotes"),
          th(""),
          th(""),
        ),
        tr(
          td.render.reattach(messageSignal),
          td.render.reattach(upvotesSignal),
          td.render.reattach(downvotesSignal),
          td(upvoteButtonData),
          td(downvoteButtonData),
        )
      ),
      p(messageField)
    ).render

  }

}