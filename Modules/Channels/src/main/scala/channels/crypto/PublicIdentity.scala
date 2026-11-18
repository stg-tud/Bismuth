package channels.crypto

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonWriter}

import java.util.Base64

case class PublicIdentity(id: String) {
  require(id.length == 44 && id.endsWith("="))

  def rawPublicKeyBytes: Array[Byte] = Base64.getDecoder.decode(id)

  // def toUid: Uid = Uid(id)
}

object PublicIdentity {
  private val base64Encoder = Base64.getEncoder

  given publicIdentityKeyCodec: JsonKeyCodec[PublicIdentity] = new JsonKeyCodec[PublicIdentity]:
      override def decodeKey(in: JsonReader): PublicIdentity               = PublicIdentity(in.readKeyAsString())
      override def encodeKey(pubId: PublicIdentity, out: JsonWriter): Unit = out.writeKey(pubId.id)

  def fromRawPublicKeyBytes(pubKeyBytes: Array[Byte]): PublicIdentity =
      require(pubKeyBytes.length == 32)
      PublicIdentity(base64Encoder.encodeToString(pubKeyBytes))

  // def fromUid(uid: Uid): PublicIdentity = PublicIdentity(uid.delegate)

  // extension (uid: Uid) inline def toPublicIdentity: PublicIdentity = PublicIdentity.fromUid(uid)
}
