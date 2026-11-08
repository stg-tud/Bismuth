package channels

import channels.JsoniterCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import channels.crypto.PublicIdentity
import channels.crypto.{Hash, Signature}
import rdts.filters.PermissionTree
import channels.broadcast.authz.ArdtEvent

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

  given ardtEventCodec: JsonValueCodec[ArdtEvent] = JsonCodecMaker.make
}
