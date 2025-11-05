package ex2024travel.lofi_acl.sync.bft

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import crypto.channels.PrivateIdentity
import crypto.{Ed25519Util, PublicIdentity}
import ex2024travel.lofi_acl.sync.bft.BftAclOpGraph.Signature
import rdts.filters.PermissionTree

import java.security.PrivateKey
import java.util.Base64
import scala.collection.mutable

// TODO: Add cached lookups (maybe keep ~5 latest and/or a few last read acls)
case class BftAclOpGraph(root: Signature, ops: Map[Signature, AclOp], heads: Set[Signature]) {
  def delegateAccess(
      delegator: PublicIdentity,
      delegatorKey: PrivateKey,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): (BftAclOpGraph, SerializedAclOp) =
      require(write <= read) // Write access implies read access! (Not really enforced)
      val op = DelegationOp(delegator, delegatee, read, write, parents = heads)
      require(isDelegationLegal(op))
      val serializedOp = op.sign(delegatorKey)
      val sigAsString  = serializedOp.signatureAsString
      (BftAclOpGraph(root, ops + (sigAsString -> op), Set(sigAsString)), serializedOp)

  def isDelegationLegal(op: DelegationOp): Boolean =
      val referenceVersion = reconstruct(op.parents).get
      op.read <= referenceVersion.read.getOrElse(op.author, PermissionTree.empty) &&
      op.write <= referenceVersion.write.getOrElse(op.author, PermissionTree.empty)

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

      // Check invariants
      decodedOp match
          case delegation @ DelegationOp(delegator, delegatee, read, write, parents) =>
            // Write access implies read access!
            require(write <= read)
            // Check delegation is valid
            require(isDelegationLegal(delegation))
          case removal @ RemovalOp(author, removed, parents) =>
            val aclAtRemoval = reconstruct(parents).get
            // Check if author is removed in specified (!) version

            // Check author is admin in ACL defined by "parents"
            require(aclAtRemoval.write(author) == PermissionTree.allow)
            require(aclAtRemoval.read(author) == PermissionTree.allow)

      // Track new op, remove heads that op references as predecessors and add new op as head
      Right(BftAclOpGraph(
        root,
        ops + (signatureAsString -> decodedOp),
        (heads -- decodedOp.parents) + signatureAsString
      ))

  def reconstruct: BftAcl = reconstruct(heads).get

  def reconstruct(heads: Set[Signature]): Option[BftAcl] =
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
