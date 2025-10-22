package ex2024travel.lofi_acl.sync.bft

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.{Ed25519Util, PublicIdentity}
import ex2024travel.lofi_acl.sync.bft.AclOp.opCodec
import ex2024travel.lofi_acl.sync.bft.BftAclOpGraph.Signature
import rdts.filters.PermissionTree

import java.security.PrivateKey
import java.util.Base64
import scala.util.{Failure, Success, Try}

sealed trait AclOp:
  val author: PublicIdentity
  val parents: Set[Signature]
  def serialize(signature: Signature): SerializedAclOp = {
    val opBytes        = writeToArray(this)
    val signatureBytes = Base64.getDecoder.decode(signature)
    // assert(Ed25519Util.checkEd25519Signature(opBytes, signatureBytes, author))
    SerializedAclOp(signatureBytes, opBytes)
  }

  def sign(delegatorKey: PrivateKey): SerializedAclOp = {
    val opBytes = writeToArray(this)
    val sig     = Ed25519Util.sign(opBytes, delegatorKey)
    SerializedAclOp(sig, opBytes)
  }

object AclOp:
  given opCodec: JsonValueCodec[AclOp] = JsonCodecMaker.make(
    CodecMakerConfig.withAllowRecursiveTypes(true) // Required for PermissionTree
  )

case class SerializedAclOp(signature: Array[Byte], op: Array[Byte]) {
  def deserialize: Try[(Signature, AclOp)] = {
    val aclOp: AclOp = readFromArray(op)
    if !Ed25519Util.checkEd25519Signature(op, signature, aclOp.author)
    then Failure(InvalidSignatureException)
    else Success((Base64.getEncoder.encodeToString(signature), aclOp))
  }

  def signatureAsString: String = Base64.getEncoder.encodeToString(signature)
}

case class DelegationOp(
    author: PublicIdentity,
    delegatee: PublicIdentity,
    read: PermissionTree,
    write: PermissionTree,
    parents: Set[Signature]
) extends AclOp

case class RemovalOp(author: PublicIdentity, removed: PublicIdentity, parents: Set[Signature]) extends AclOp

object InvalidSignatureException extends RuntimeException
