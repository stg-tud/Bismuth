package test.rdts.bespoke

import rdts.experiments.UndoRedoReplica
import rdts.base.{Uid, LocalUid, Lattice, Bottom}

def createTestReplicas[A](n: Int): Array[UndoRedoReplica[A]] = {
  (1 to n).map(i => Uid.predefined(s"R$i")).map(uid =>
    UndoRedoReplica.empty(using LocalUid(uid))
  ).toArray
}

class UndoRedoReplicaTest extends munit.FunSuite {
  test("simple undo redo") {
    case class State(value: Int) {
      def setValue(v: Int): State = this.copy(value = v)
    }

    object State {
      given lattice: Lattice[State] with
        def merge(a: State, b: State): State = State(value = math.max(a.value, b.value))

      given bottom: Bottom[State] = Bottom.provide(State(value = 0))
    }

    val Array(replica) = createTestReplicas[State](1)

    replica.mod(_.setValue(1))
    assertEquals(replica.state, State(value = 1))

    replica.mod(_.setValue(2))
    assertEquals(replica.state, State(value = 2))

    replica.undo()
    assertEquals(replica.state, State(value = 1))

    replica.undo()
    assertEquals(replica.state, State.bottom.empty)

    replica.redo()
    assertEquals(replica.state, State(value = 1))

    replica.redo()
    assertEquals(replica.state, State(value = 2))
  }

  test("social media example") {
    import rdts.datatypes.{ReplicatedList, GrowOnlyCounter as Counter, LastWriterWins as LWW, ObserveRemoveMap}

    type ID = String

    case class SocialMedia(sm: ObserveRemoveMap[ID, SocialPost] = ObserveRemoveMap.empty):
      def post_views: Map[ID, SocialPostView] =
        sm.inner.view.mapValues(post => SocialPostView.from(post.value)).toMap

      def like(post: ID)(using replicaId: LocalUid): SocialMedia =
        val increment = sm.inner(post).value.likes.inc()
        SocialMedia(sm.update(post, SocialPost(likes = increment)))

      def comment(post: ID, text: String)(using replicaId: LocalUid): SocialMedia =
        val comments = sm.inner(post).value.comments.append(text)
        SocialMedia(sm.update(post, SocialPost(comments = comments)))

      def post(id: String, text: String)(using replicaId: LocalUid): SocialMedia =
        SocialMedia(sm.update(id, SocialPost(message = Some(LWW.now(text)))))

    case class SocialPost(
        message: Option[LWW[String]] = None,
        comments: ReplicatedList[String] = ReplicatedList.empty,
        likes: Counter = Counter.zero,
        dislikes: Counter = Counter.zero
    )

    case class SocialPostView(
        message: Option[String] = None,
        comments: List[String] = List.empty,
        likes: Int = 0,
        dislikes: Int = 0
    )

    object SocialPostView {
      def from(post: SocialPost): SocialPostView =
        SocialPostView(
          message = post.message.map(_.value),
          comments = post.comments.toList,
          likes = post.likes.value,
          dislikes = post.dislikes.value
        )
    }

    object SocialMedia {
      given lattice: Lattice[SocialMedia] = Lattice.derived
      given bottom: Bottom[SocialMedia]   = Bottom.derived
    }

    object SocialPost {
      given lattice: Lattice[SocialPost] = Lattice.derived
      given bottom: Bottom[SocialPost]   = Bottom.derived
    }

    val Array(replica1, replica2) = createTestReplicas[SocialMedia](2)

    val post1 = "post1"
    val post2 = "post2"

    val delta1 = replica1.mod(_.post(post1, "Post from Replica 1!"))
    val delta2 = replica2.mod(_.post(post2, "Post from Replica 2!"))

    assertEquals(replica1.state.post_views, Map(post1 -> SocialPostView(message = Some("Post from Replica 1!"))))
    assertEquals(replica2.state.post_views, Map(post2 -> SocialPostView(message = Some("Post from Replica 2!"))))

    replica1.receive(delta2)
    replica2.receive(delta1)

    assertEquals(
      replica1.state.post_views,
      Map(
        post1 -> SocialPostView(message = Some("Post from Replica 1!")),
        post2 -> SocialPostView(message = Some("Post from Replica 2!"))
      )
    )
    assertEquals(
      replica2.state.post_views,
      Map(
        post2 -> SocialPostView(message = Some("Post from Replica 2!")),
        post1 -> SocialPostView(message = Some("Post from Replica 1!"))
      )
    )

    val delta3 = replica1.mod(_.like(post1))
    replica2.receive(delta3)

    assertEquals(
      replica1.state.post_views,
      Map(
        post1 -> SocialPostView(message = Some("Post from Replica 1!"), likes = 1),
        post2 -> SocialPostView(message = Some("Post from Replica 2!"))
      )
    )
    assertEquals(
      replica2.state.post_views,
      Map(
        post1 -> SocialPostView(message = Some("Post from Replica 1!"), likes = 1),
        post2 -> SocialPostView(message = Some("Post from Replica 2!")),
      )
    )

    val delta4 = replica1.undo()
    val delta5 = replica2.mod(_.like(post1))

    assertEquals(
      replica1.state.post_views,
      Map(
        post1 -> SocialPostView(message = Some("Post from Replica 1!")),
        post2 -> SocialPostView(message = Some("Post from Replica 2!"))
      )
    )
    assertEquals(
      replica2.state.post_views,
      Map(
        post1 -> SocialPostView(message = Some("Post from Replica 1!"), likes = 2),
        post2 -> SocialPostView(message = Some("Post from Replica 2!")),
      )
    )

    replica1.receive(delta5)
    replica2.receive(delta4)

    assertEquals(
      replica1.state.post_views,
      Map(
        post1 -> SocialPostView(message = Some("Post from Replica 1!"), likes = 1),
        post2 -> SocialPostView(message = Some("Post from Replica 2!"))
      )
    )
    assertEquals(
      replica2.state.post_views,
      Map(
        post1 -> SocialPostView(message = Some("Post from Replica 1!"), likes = 1),
        post2 -> SocialPostView(message = Some("Post from Replica 2!")),
      )
    )

    val delta6 = replica1.undo()
    replica2.receive(delta6)

    assertEquals(
      replica1.state.post_views,
      Map(
        post2 -> SocialPostView(message = Some("Post from Replica 2!"))
      )
    )
    assertEquals(
      replica2.state.post_views,
      Map(
        post2 -> SocialPostView(message = Some("Post from Replica 2!")),
      )
    )
  }
}
