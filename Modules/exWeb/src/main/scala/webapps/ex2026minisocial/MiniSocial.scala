package webapps.ex2026minisocial

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.LastWriterWins

/** Replicated state of the mini social example: a message and two (up/down) vote counters. */
case class MiniSocial(
    message: LastWriterWins[String] = LastWriterWins.fallback(""),
    upvotes: GrowOnlyCounter = GrowOnlyCounter.zero,
    downvotes: GrowOnlyCounter = GrowOnlyCounter.zero
) {
  def like()(using LocalUid): MiniSocial =
    MiniSocial(upvotes = upvotes.add(1))

  def dislike()(using LocalUid): MiniSocial =
    MiniSocial(downvotes = downvotes.add(1))

  def setMessage(newMessage: String): MiniSocial =
    MiniSocial(message = message.write(newMessage))
}

object MiniSocial {
  given Lattice[MiniSocial] = Lattice.derived

  // needed by the replication manager to handle some special cases
  given bottom: Bottom[MiniSocial] = Bottom.provide(MiniSocial())

  // this is for serialization to JSON
  given codec: JsonValueCodec[MiniSocial] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}