package webapps.ex2024socialMedia

import _root_.ex2024socialMedia.{ID, SocialMedia, SocialPost}
import org.scalajs.dom
import rdts.base.{Lattice, ReplicaId}
import rdts.syntax.DeltaBuffer
import reactives.default.*
import scalatags.JsDom.all.*
import webapps.{DemoBridge, OverlayReplication}

import scala.collection.mutable
import scala.scalajs.js.annotation.JSExportTopLevel

object SocialMediaMain {

  given replicaId: ReplicaId = ReplicaId.gen()

  /** All local changes, as functions producing a delta. */
  val actions: Evt[SocialMedia => SocialMedia] = Evt()

  @JSExportTopLevel("SocialMedia")
  def run(): Unit = {
    val (received, replication) = Event.fromCallback {
      OverlayReplication[SocialMedia](replicaId, delta => Event.handle(delta))
    }

    val init = replication.appPayloads.foldLeft(SocialMedia())(Lattice.merge)

    val state: Signal[DeltaBuffer[SocialMedia]] = Fold(DeltaBuffer(init))(
      Fold.Branch[DeltaBuffer[SocialMedia]](Nil, isStatic = false, _ => Fold.current.clearDeltas()),
      actions.branch(f => Fold.current.mod(f)),
      received.branch(delta => Fold.current.applyDeltaNonAppend(delta)),
    )
    state.observe(_.deltaBuffer.foreach(replication.broadcast))

    val feed = div(cls := "feed").render
    val views = mutable.Map.empty[ID, PostView]
    state.map(_.state).observe { social =>
      // keep existing post elements (and thus their comment inputs), only add new ones and update contents
      val ordered = social.posts.map { (id, post) =>
        val view = views.getOrElseUpdate(id, PostView(id))
        view.update(post)
        view.element
      }
      if feed.children.length != ordered.size then feed.replaceChildren(ordered*)
    }

    val compose = textarea(placeholder := "What’s happening?", rows := 2).render
    def submit(): Unit =
      if compose.value.trim.nonEmpty then
          val text = compose.value.trim
          compose.value = ""
          actions.fire(_.post(text))

    val content = div(
      cls := "social-media",
      form(
        cls := "compose",
        onsubmit := { (ev: dom.Event) => ev.preventDefault(); submit() },
        compose,
        button(`type` := "submit", "Post"),
      ),
      feed,
    ).render

    val container = dom.document.getElementById("app")
    container.replaceChildren(content, DemoBridge.connectionView(replication.io))
  }

  /** The element of a single post, updated in place. */
  class PostView(id: ID) {
    private val message  = p(cls := "post-message").render
    private val likes    = button(cls := "vote", onclick := { () => actions.fire(_.like(id)) }).render
    private val dislikes = button(cls := "vote", onclick := { () => actions.fire(_.dislike(id)) }).render
    private val comments = ul(cls := "comments").render
    private val input    = input_(placeholder := "Write a comment …").render

    val element: dom.Element = scalatags.JsDom.tags2.article(
      cls := "post",
      message,
      div(cls := "votes", likes, dislikes),
      comments,
      form(
        cls := "comment-form",
        onsubmit := { (ev: dom.Event) =>
          ev.preventDefault()
          val text = input.value.trim
          if text.nonEmpty then
              input.value = ""
              actions.fire(_.comment(id, text))
        },
        input,
        button(`type` := "submit", "Reply"),
      ),
    ).render

    def update(post: SocialPost): Unit = {
      message.textContent = post.message.map(_.value).getOrElse("")
      likes.textContent = s"${Character.toString(0x1f44d)} ${post.likes.value}"
      dislikes.textContent = s"${Character.toString(0x1f44e)} ${post.dislikes.value}"
      val texts = post.comments.toList
      if comments.children.length != texts.size then
          comments.replaceChildren(texts.map(t => li(t).render)*)
    }
  }

  private def input_ = scalatags.JsDom.all.input
}
