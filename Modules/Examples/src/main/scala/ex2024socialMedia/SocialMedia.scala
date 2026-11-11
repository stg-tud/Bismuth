package ex2024socialMedia

import channels.JsoniterCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.{Bottom, Lattice, ReplicaId, Uid}
import rdts.datatypes.{ReplicatedList, GrowOnlyCounter as Counter, LastWriterWins as LWW}

/** Post ids start with the creation time, so ordering by id roughly orders by time. */
type ID = String

/** A small social network: posts that can be liked, disliked, and commented on.
  * All mutators return deltas.
  */
case class SocialMedia(sm: Map[ID, SocialPost] = Map.empty):
    def posts: List[(ID, SocialPost)] = sm.toList.sortBy(_._1).reverse

    private def at(post: ID): SocialPost = sm.getOrElse(post, SocialPost())

    def like(post: ID)(using ReplicaId): SocialMedia =
      SocialMedia(Map(post -> SocialPost(likes = at(post).likes.inc())))

    def dislike(post: ID)(using ReplicaId): SocialMedia =
      SocialMedia(Map(post -> SocialPost(dislikes = at(post).dislikes.inc())))

    def comment(post: ID, text: String)(using ReplicaId): SocialMedia =
      SocialMedia(Map(post -> SocialPost(comments = at(post).comments.append(text))))

    def post(text: String)(using replicaId: ReplicaId): SocialMedia =
      val id = f"${System.currentTimeMillis()}%015d-${Uid.unwrap(replicaId.uid)}"
      SocialMedia(Map(id -> SocialPost(message = Some(LWW.now(text)))))

object SocialMedia:
    given Lattice[SocialMedia]        = Lattice.derived
    given Bottom[SocialMedia]         = Bottom.derived
    given JsonValueCodec[SocialMedia] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

case class SocialPost(
    message: Option[LWW[String]] = None,
    comments: ReplicatedList[String] = ReplicatedList.empty,
    likes: Counter = Counter.zero,
    dislikes: Counter = Counter.zero
)

object SocialPost:
    given Lattice[SocialPost] = Lattice.derived
    given Bottom[SocialPost]  = Bottom.derived
