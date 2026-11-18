package channels.crypto

import java.util.Base64

case class PublicIdentity(id: String) {
  require(id.length == 44 && id.endsWith("="))

  def rawPublicKeyBytes: Array[Byte] = Base64.getDecoder.decode(id)

  // def toUid: Uid = Uid(id)
}

object PublicIdentity {
  private val base64Encoder = Base64.getEncoder

  def fromRawPublicKeyBytes(pubKeyBytes: Array[Byte]): PublicIdentity =
      require(pubKeyBytes.length == 32)
      PublicIdentity(base64Encoder.encodeToString(pubKeyBytes))

  // def fromUid(uid: Uid): PublicIdentity = PublicIdentity(uid.delegate)

  // extension (uid: Uid) inline def toPublicIdentity: PublicIdentity = PublicIdentity.fromUid(uid)
}
