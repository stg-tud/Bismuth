package lofi_acl.sync

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.PublicIdentity
import lofi_acl.bft.{Hash, Signature}
import lofi_acl.sync.signed.SignedDelta
import lofi_acl.sync.signed.FilteredRdtSync.SyncMsg
import rdts.filters.PermissionTree
import replication.JsoniterCodecs.given

object JsoniterCodecs {
  given pubIdentityKeyCodec: JsonKeyCodec[PublicIdentity] = new JsonKeyCodec[PublicIdentity]:
      override def decodeKey(in: JsonReader): PublicIdentity               = PublicIdentity(in.readKeyAsString())
      override def encodeKey(pubId: PublicIdentity, out: JsonWriter): Unit = out.writeKey(pubId.id)

  given hashValueCodec: JsonValueCodec[Hash] = Hash.hashValueCodec

  given hashKeyCodec: JsonKeyCodec[Hash] = Hash.hashKeyCodec

  given signatureValueCodec: JsonValueCodec[Signature] = Signature.signatureValueCodec

  given signatureKeyCodec: JsonKeyCodec[Signature] = Signature.signatureKeyCodec

  given permissionTreeCodec: JsonValueCodec[PermissionTree] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapAsArray(true)
      .withAllowRecursiveTypes(true)
  )

  given filterableSignedDeltaCodec[State: JsonValueCodec]: JsonValueCodec[SignedDelta[State]] =
    JsonCodecMaker.make

  given syncMsgCodec[State: JsonValueCodec]: JsonValueCodec[SyncMsg[State]] =
    JsonCodecMaker.make
}
