package ex2021encfxtodo

import channels.broadcast.PlumtreeMessage
import channels.{BroadcastIO, experiments}
import channels.connection.SynchronousLocalConnection
import channels.experiments.Aead
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{KeyTemplates, KeysetHandle, RegistryConfiguration, Aead as TinkAead}
import munit.FunSuite
import rdts.base.LocalUid

import scala.collection.mutable

class BroadcastIOAeadTest extends FunSuite {

  given JsonValueCodec[Set[String]] = JsonCodecMaker.make

  private def newAead(): Aead = {
    AeadConfig.register()
    val keyset = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
    val aead   = keyset.getPrimitive(RegistryConfiguration.get(), classOf[TinkAead])
    TinkBasedAead(aead)
  }

  test("BroadcastIO disseminates over a real AEAD") {
    val aead = newAead()

    val received1 = mutable.ListBuffer.empty[Set[String]]
    val received2 = mutable.ListBuffer.empty[Set[String]]

    val dd1 = BroadcastIO[Set[String]](LocalUid.gen(), received1 += _, aead = aead)
    val dd2 = BroadcastIO[Set[String]](LocalUid.gen(), received2 += _, aead = aead)

    val sync = SynchronousLocalConnection("sync-aead")
    dd1.addServerConnection(sync.server)
    dd2.addClientConnection(sync.client("2"))

    dd1.broadcast(Set("secret"))

    assertEquals(dd1.allPayloads.map(_.data).toSet, Set(Set("secret")))
    assertEquals(dd2.allPayloads.map(_.data).toSet, Set(Set("secret")))
    assert(received2.contains(Set("secret")))
  }

  test("AEAD encrypted envelopes are not readable with the identity AEAD") {
    val aead    = newAead()
    val encoded = BroadcastIO.encodeEnvelope[Set[String]](
      BroadcastIO.Envelope.Broadcast(LocalUid.gen().uid, PlumtreeMessage.Graft(rdts.time.Dots.empty)),
      aead
    )

    assert(BroadcastIO.decodeEnvelope[Set[String]](encoded, experiments.Aead.identity).isFailure)
    assert(BroadcastIO.decodeEnvelope[Set[String]](encoded, aead).isSuccess)
  }
}
