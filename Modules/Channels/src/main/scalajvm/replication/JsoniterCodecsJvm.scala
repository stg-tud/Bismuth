package replication

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.{Hash, PublicIdentity, Signature}
import rdts.filters.PermissionTree
import replication.acl.sync.anti_entropy.AclEnforcingSync.SyncMsg
import replication.acl.sync.anti_entropy.SignedDelta
import channels.JsoniterCodecs.given
import replication.authz.ArdtEvent

object JsoniterCodecsJvm {
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

  given ardtEventCodec: JsonValueCodec[ArdtEvent] = JsonCodecMaker.make
}
