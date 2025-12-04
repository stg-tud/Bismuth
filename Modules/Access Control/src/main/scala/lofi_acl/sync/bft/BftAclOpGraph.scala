package lofi_acl.sync.bft

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import crypto.channels.PrivateIdentity
import crypto.{Ed25519Util, PublicIdentity}
import BftAclOpGraph.Signature
import rdts.filters.PermissionTree

import java.security.PrivateKey
import java.util.Base64
import scala.collection.mutable

// TODO: Add cached lookups (maybe keep ~5 latest and/or a few last read acls)
class BftAclOpGraph(
    val root: Signature,
    val ops: Map[Signature, AclOp],
    val heads: Set[Signature],
    val latestAcl: BftAcl
) {
  private def applyOp(signature: Signature, op: AclOp): BftAclOpGraph =
      require(isOpLegal(op))
      val updatedAcl = op match {
        case DelegationOp(_, delegatee, read, write, _) => latestAcl.addPermissions(delegatee, read, write)
        case RemovalOp(_, removed, _)                   => latestAcl.remove(removed)
      }
      BftAclOpGraph(root, ops + (signature -> op), (heads -- op.parents) + signature, updatedAcl)

  def delegateAccess(
      delegator: PublicIdentity,
      delegatorKey: PrivateKey,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): (BftAclOpGraph, SerializedAclOp) =
      val op           = DelegationOp(delegator, delegatee, read, write, parents = heads)
      val serializedOp = op.sign(delegatorKey)
      val sigAsString  = serializedOp.signatureAsString
      (applyOp(sigAsString, op), serializedOp)

  private def isOpLegal(op: AclOp): Boolean =
      val opPrefixAcl = reconstruct(op.parents).get
      op.parents.nonEmpty && // Ensure that we don't accept a second root (node without predecessors)
      !opPrefixAcl.removed.contains(op.author) &&
      (op match
          case delegation @ DelegationOp(delegator, delegatee, read, write, parents) =>
            !opPrefixAcl.removed.contains(delegation.delegatee) && // Delegatee must be a member
            write <= read &&                                       // Write access should imply read access
            // read and write permissions of author must be less or equal to delegated permissions
            delegation.read <= opPrefixAcl.read.getOrElse(op.author, PermissionTree.empty) &&
            delegation.write <= opPrefixAcl.write.getOrElse(op.author, PermissionTree.empty)
          case removal @ RemovalOp(author, removed, parents) =>
            // Check author is admin in ACL defined by "parents"
            opPrefixAcl.write(author) == PermissionTree.allow &&
            opPrefixAcl.read(author) == PermissionTree.allow)

  // Returns either missing deltas, or the (potentially) updated op graph
  def receive(serializedOp: SerializedAclOp): Either[Set[Signature], BftAclOpGraph] =
      val signatureAsString = Base64.getEncoder.encodeToString(serializedOp.signature)
      if ops.contains(signatureAsString) then return Right(this)
      val decodedOp = readFromArray[AclOp](serializedOp.op)

      // Check signature
      if !Ed25519Util.checkEd25519Signature(serializedOp.op, serializedOp.signature, decodedOp.author)
      then throw InvalidSignatureException

      // Check preceding ops are already applied
      val missing = decodedOp.parents.filterNot(ops.contains)
      if missing.nonEmpty then return Left(missing)

      Right(applyOp(signatureAsString, decodedOp))

  def reconstruct: BftAcl = reconstruct(heads).get

  def reconstruct(heads: Set[Signature]): Option[BftAcl] =
      if heads == this.heads then return Some(latestAcl)
      require(heads.forall(ops.contains))

      val visited   = mutable.Set.empty[Signature]
      val toMerge   = mutable.Stack.from(heads)
      var resultAcl = BftAcl(Map.empty, Map.empty)

      while toMerge.nonEmpty do {
        val next = toMerge.pop()
        if !visited.contains(next) then
            val op = ops(next)
            visited += next
            toMerge ++= op.parents.diff(visited)
            op match
                case DelegationOp(_, delegatee, read, write, _) =>
                  resultAcl = resultAcl.addPermissions(delegatee, read, write)
                case RemovalOp(_, removed, _) =>
                  resultAcl = resultAcl.remove(removed)
      }

      Some(resultAcl)
}

object BftAclOpGraph:
    type Signature = String

    def fromRootAclOp(signature: Signature, aclOp: AclOp): BftAclOpGraph = {
      aclOp match {
        case DelegationOp(author, delegatee, read, write, parents) =>
          require(author == delegatee)
          require(read == PermissionTree.allow)
          require(write == PermissionTree.allow)
          require(parents.isEmpty)
          BftAclOpGraph(
            signature,
            Map(signature -> aclOp),
            Set(signature),
            BftAcl(Map(delegatee -> read), Map(delegatee -> write), Set.empty)
          )
        case RemovalOp(author, removed, parents) => throw IllegalArgumentException("root op must be a delegation")
      }
    }

    def createSelfSignedRoot(rootIdentity: PrivateIdentity): SerializedAclOp = {
      val rootAclDelta: DelegationOp = DelegationOp(
        author = rootIdentity.getPublic,
        delegatee = rootIdentity.getPublic,
        read = PermissionTree.allow,
        write = PermissionTree.allow,
        parents = Set.empty
      )
      val opBytes = writeToArray[AclOp](rootAclDelta)
      val sig     = Ed25519Util.sign(opBytes, rootIdentity.identityKey.getPrivate)
      SerializedAclOp(sig, opBytes)
    }
