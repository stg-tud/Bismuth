package ex2021encfxtodo

import channels.SynchronousLocalConnection
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead => TinkAead, KeyTemplates, KeysetHandle, RegistryConfiguration}
import munit.FunSuite
import rdts.base.LocalUid
import replication.{BroadcastIO, PlumtreeMessage}

import scala.collection.mutable

class BroadcastIOAeadTest extends FunSuite {

  given JsonValueCodec[Set[String]] = JsonCodecMaker.make

  private def newAead(): replication.Aead = {
    AeadConfig.register()
    val keyset = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
    val aead   = keyset.getPrimitive(RegistryConfiguration.get(), classOf[TinkAead])
    AeadTranslation(aead)
  }

  test("BroadcastIO disseminates over a real AEAD") {
    val aead = newAead()

    val received1 = mutable.ListBuffer.empty[Set[String]]
    val received2 = mutable.ListBuffer.empty[Set[String]]

    val dd1 = BroadcastIO[Set[String]](LocalUid.gen(), received1 += _, aead = aead)
    val dd2 = BroadcastIO[Set[String]](LocalUid.gen(), received2 += _, aead = aead)

    val sync = SynchronousLocalConnection()
    dd1.addServerConnection(sync.server)
    dd2.addClientConnection(sync.client("2"))

    dd1.applyDelta(Set("secret"))

    assertEquals(dd1.allPayloads.map(_.data).toSet, Set(Set("secret")))
    assertEquals(dd2.allPayloads.map(_.data).toSet, Set(Set("secret")))
    assert(received2.contains(Set("secret")))
  }

  test("AEAD encrypted envelopes are not readable with the identity AEAD") {
    val aead = newAead()
    val encoded = BroadcastIO.encodeEnvelope[Set[String]](
      BroadcastIO.Envelope.Broadcast(LocalUid.gen().uid, PlumtreeMessage.Graft(rdts.time.Dots.empty)),
      aead
    )

    assert(BroadcastIO.decodeEnvelope[Set[String]](encoded, replication.Aead.identity).isFailure)
    assert(BroadcastIO.decodeEnvelope[Set[String]](encoded, aead).isSuccess)
  }
}
