package ex2024travel.lofi_acl.sync.bft

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.channels.PrivateIdentity
import crypto.{Ed25519Util, PublicIdentity}
import BftAclOpGraph.{Delegation, EncodedDelegation, Signature, opCodec}
import ex2024travel.lofi_acl.sync.Acl
import rdts.filters.PermissionTree

import java.security.PrivateKey
import java.util.Base64
import scala.collection.mutable
import scala.runtime.Arrays
import scala.util.{Failure, Success, Try}

case class BftAclOpGraph(root: Signature, ops: Map[Signature, Delegation], heads: Set[Signature]) {
  def delegateAccess(
      delegator: PublicIdentity,
      delegatorKey: PrivateKey,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): (BftAclOpGraph, EncodedDelegation) =
    require(write <= read) // Write access implies read access! (Not really enforced)
    val op = Delegation(delegator, delegatee, read, write, parents = heads)
    require(isDelegationLegal(op))
    val opBytes     = writeToArray(op)
    val sig         = Ed25519Util.sign(opBytes, delegatorKey)
    val sigAsString = Base64.getEncoder.encodeToString(sig)
    (BftAclOpGraph(root, ops + (sigAsString -> op), Set(sigAsString)), EncodedDelegation(sig, opBytes))

  def isDelegationLegal(op: Delegation): Boolean =
    val referenceVersion = reconstruct(op.parents).get
    op.read <= referenceVersion.read.getOrElse(op.delegator, PermissionTree.empty) &&
    op.write <= referenceVersion.write.getOrElse(op.delegator, PermissionTree.empty)

  def receive(signature: Array[Byte], encodedOp: Array[Byte]): Either[Set[Signature], BftAclOpGraph] =
    val signatureAsString = Base64.getEncoder.encodeToString(signature)
    if ops.contains(signatureAsString) then return Right(this)
    readFromArray[Delegation](encodedOp) match
      case delegation @ Delegation(delegator, delegatee, read, write, parents) =>
        if !Ed25519Util.checkEd25519Signature(encodedOp, signature, delegator) then throw InvalidSignatureException
        // Write access implies read access!
        require(write <= read)
        // Check preceding ops are already applied
        val missing = parents.filterNot(ops.contains)
        if missing.nonEmpty then return Left(missing)
        // Track new op, remove heads that op references as predecessors and add new op as head
        Right(BftAclOpGraph(root, ops + (signatureAsString -> delegation), (heads -- parents) + signatureAsString))

  def reconstruct(heads: Set[Signature]): Option[Acl] =
    require(heads.forall(ops.contains))

    val visited   = mutable.Set.empty[Signature]
    val toMerge   = mutable.Stack.from(heads)
    var resultAcl = Acl(Map.empty, Map.empty)

    while toMerge.nonEmpty do {
      val next = toMerge.pop()
      if !visited.contains(next) then
        ops(next) match
          case Delegation(_, delegatee, read, write, parents) =>
            visited += next
            toMerge ++= parents.diff(visited)
            resultAcl = resultAcl.addPermissions(delegatee, read, write)
    }

    Some(resultAcl)
}

object BftAclOpGraph:
  type Signature = String

  def createSelfSignedRoot(rootIdentity: PrivateIdentity): EncodedDelegation = {
    val rootAclDelta: Delegation = Delegation(
      delegator = rootIdentity.getPublic,
      delegatee = rootIdentity.getPublic,
      read = PermissionTree.allow,
      write = PermissionTree.allow,
      parents = Set.empty
    )
    val opBytes = writeToArray(rootAclDelta)
    val sig     = Ed25519Util.sign(opBytes, rootIdentity.identityKey.getPrivate)
    EncodedDelegation(sig, opBytes)
  }

  given opCodec: JsonValueCodec[Delegation] = JsonCodecMaker.make(
    CodecMakerConfig.withAllowRecursiveTypes(true) // Required for PermissionTree
  )

  case class Delegation(
      delegator: PublicIdentity,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree,
      parents: Set[Signature]
  ) {
    def encode(signature: Signature)(using JsonValueCodec[Delegation]): EncodedDelegation = {
      val delegationBytes = writeToArray(this)
      val signatureBytes  = Base64.getDecoder.decode(signature)
      assert(Ed25519Util.checkEd25519Signature(delegationBytes, signatureBytes, delegator))
      EncodedDelegation(signatureBytes, delegationBytes)
    }
  }

  case class EncodedDelegation(sig: Array[Byte], op: Array[Byte]) {
    def decode(using JsonValueCodec[Delegation]): Try[(Signature, Delegation)] = {
      val delegation: Delegation = readFromArray(op)
      val signer                 = delegation.delegator.publicKey
      if !Ed25519Util.checkEd25519Signature(op, sig, signer)
      then Failure(InvalidSignatureException)
      else Success((Base64.getEncoder.encodeToString(sig), delegation))
    }
  }

object InvalidSignatureException extends RuntimeException
