package lofi_acl.eval

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.Ed25519Util
import crypto.channels.PrivateIdentity
import lofi_acl.sync.bft.BftAclOpGraph.Signature
import SavedTrace.{DeltaTrace, NotificationTrace}
import TravelPlannerBenchmark.RDT
import lofi_acl.sync.bft.{AclOp, SerializedAclOp}
import org.bouncycastle.cert.X509CertificateHolder
import replication.JsoniterCodecs

import java.security.KeyPair

case class SavedTrace(
    numOps: Int,
    identities: Array[PrivateIdentity],
    aclRoot: SerializedAclOp,
    aclOps: Map[Signature, AclOp],
    connectionMap: Map[Int, Set[Int]],
    deltaTrace: DeltaTrace,
    notificationTrace: NotificationTrace
)

object SavedTrace {
  type DeltaTrace        = Vector[Seq[(Int, RDT)]]
  type NotificationTrace = Array[Array[Int]]

  given privateIdentityCodec: JsonValueCodec[SavedTrace] = {
    given JsonValueCodec[KeyPair] = JsoniterCodecs.bimapCodec[Array[Byte], KeyPair](
      JsonCodecMaker.make,
      arr => if arr.isEmpty then null else Ed25519Util.rawPrivateKeyBytesToKeyPair(arr),
      keypair =>
        if keypair eq null then Array.empty else Ed25519Util.privateKeyToRawPrivateKeyBytes(keypair.getPrivate)
    )

    given JsonValueCodec[X509CertificateHolder] = JsoniterCodecs.bimapCodec[Array[Byte], X509CertificateHolder](
      JsonCodecMaker.make,
      encoded => if encoded.isEmpty then null else X509CertificateHolder(encoded),
      cert => if cert eq null then Array.empty else cert.getEncoded
    )

    JsonCodecMaker.make
  }
}
