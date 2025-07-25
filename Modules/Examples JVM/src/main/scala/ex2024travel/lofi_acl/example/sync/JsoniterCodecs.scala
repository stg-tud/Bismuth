package ex2024travel.lofi_acl.example.sync

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.PublicIdentity
import ex2024travel.lofi_acl.example.sync.acl.monotonic.MonotonicAclSyncMessage
import MonotonicAclSyncMessage.Signature
import rdts.base.Uid
import rdts.time.Dots

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

  given dotsCodec: JsonValueCodec[Dots] = JsonCodecMaker.make

  given messageJsonCodec[RDT: JsonValueCodec]: JsonValueCodec[MonotonicAclSyncMessage[RDT]] = JsonCodecMaker.make(
    CodecMakerConfig
      .withAllowRecursiveTypes(true) // Required for PermissionTree
      .withMapAsArray(true)
  )
}
