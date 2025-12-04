package lofi_acl.sync

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.PublicIdentity
import lofi_acl.sync.monotonic.MonotonicAclSyncMessage
import MonotonicAclSyncMessage.Signature
import rdts.base.{Bottom, Uid}
import rdts.datatypes.LastWriterWins
import rdts.filters.PermissionTree
import rdts.time.{CausalTime, Dots}

object JsoniterCodecs {
  given uidKeyCodec: JsonKeyCodec[rdts.base.Uid] = new JsonKeyCodec[Uid]:
      override def decodeKey(in: JsonReader): Uid             = Uid(in.readKeyAsString())
      override def encodeKey(uid: Uid, out: JsonWriter): Unit = out.writeKey(uid.delegate)

  given pubIdentityKeyCodec: JsonKeyCodec[PublicIdentity] = new JsonKeyCodec[PublicIdentity]:
      override def decodeKey(in: JsonReader): PublicIdentity               = PublicIdentity(in.readKeyAsString())
      override def encodeKey(pubId: PublicIdentity, out: JsonWriter): Unit = out.writeKey(pubId.id)

  given signatureCodec: JsonValueCodec[Signature | Null] = new JsonValueCodec[Signature | Null]:
      override def decodeValue(in: JsonReader, default: Signature | Null): Signature | Null =
          val sigArray = in.readBase64AsBytes(Array.empty)
          if sigArray.isEmpty then null
          else Signature(sigArray)
      override def encodeValue(sig: Signature | Null, out: JsonWriter): Unit =
        if sig == null then out.writeVal("")
        else out.writeBase64Val(sig.sig, true)
      override def nullValue: Signature | Null = null

  given lwwCodecWithBottomOptimization[V: {Bottom, JsonValueCodec}]: JsonValueCodec[LastWriterWins[V]] = {
    import LastWriterWins.bottom
    replication.JsoniterCodecs.bimapCodec[LastWriterWins[V], LastWriterWins[V]](
      JsonCodecMaker.make[LastWriterWins[V]],
      lww => if lww.isEmpty then null else lww,
      lww => if lww eq null then LastWriterWins.bottom.empty else lww
    )
  }

  given causalTimeCodec: JsonValueCodec[CausalTime] = replication.JsoniterCodecs.bimapCodec[Array[Long], CausalTime](
    JsonCodecMaker.make,
    arr => if arr.isEmpty then CausalTime.empty else CausalTime(arr(0), arr(1), arr(2)),
    ct => if ct.isEmpty then Array.empty else Array(ct.time, ct.causal, ct.random)
  )

  given permissionTreeCodec: JsonValueCodec[PermissionTree] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapAsArray(true)
      .withAllowRecursiveTypes(true)
  )

  given messageJsonCodec[RDT: JsonValueCodec]: JsonValueCodec[MonotonicAclSyncMessage[RDT]] = JsonCodecMaker.make(
    CodecMakerConfig.withMapAsArray(true)
  )
}
