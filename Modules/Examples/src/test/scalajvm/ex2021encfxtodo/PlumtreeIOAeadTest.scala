package ex2021encfxtodo

import channels.broadcast.PlumtreeMessage
import channels.connection.{LocalMessageQueue, QueuedLocalConnection}
import channels.experiments.Aead
import channels.{PlumtreeIO, experiments}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{KeyTemplates, KeysetHandle, RegistryConfiguration, Aead as TinkAead}
import munit.FunSuite
import rdts.base.ReplicaId

import scala.collection.mutable

class PlumtreeIOAeadTest extends FunSuite {

  given JsonValueCodec[Set[String]] = JsonCodecMaker.make

  private def newAead(): Aead = {
    AeadConfig.register()
    val keyset =
      KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305").toParameters, RegistryConfiguration.get())
    val aead = keyset.getPrimitive(RegistryConfiguration.get(), classOf[TinkAead])
    TinkBasedAead(aead)
  }

  test("PlumtreeIO disseminates over a real AEAD") {
    val aead = newAead()

    val received1 = mutable.ListBuffer.empty[Set[String]]
    val received2 = mutable.ListBuffer.empty[Set[String]]

    val dd1 = PlumtreeIO[Set[String]](ReplicaId.gen(), p => received1 += p.data, aead = aead)
    val dd2 = PlumtreeIO[Set[String]](ReplicaId.gen(), p => received2 += p.data, aead = aead)

    val queue = LocalMessageQueue()
    val link  = QueuedLocalConnection("aead", queue)
    dd1.addServerConnection(link.server)
    dd2.addClientConnection(link.client("2"))

    dd1.broadcast(Set("secret"))
    while queue.nonEmpty do queue.deliverAll(): Unit

    assertEquals(dd1.allPayloads.map(_.data).toSet, Set(Set("secret")))
    assertEquals(dd2.allPayloads.map(_.data).toSet, Set(Set("secret")))
    assert(received2.contains(Set("secret")))
  }

  test("AEAD encrypted envelopes are not readable with the identity AEAD") {
    val aead    = newAead()
    val encoded = PlumtreeIO.encodeEnvelope[Set[String]](
      PlumtreeIO.Envelope.Broadcast(channels.connection.Peer.Claimed(ReplicaId.gen().uid), PlumtreeMessage.Graft(rdts.time.Dots.empty)),
      aead
    )

    assert(PlumtreeIO.decodeEnvelope[Set[String]](encoded, experiments.Aead.identity).isFailure)
    assert(PlumtreeIO.decodeEnvelope[Set[String]](encoded, aead).isSuccess)
  }
}
