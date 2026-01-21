package lofi_acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.AclRdt.given
import lofi_acl.bft.HashDag.Encoder
import rdts.filters.PermissionTree

class AclRdt(privateIdentity: PrivateIdentity, cache: Set[Hash] => Option[Acl] = _ => None)
    extends BftSignedDeltaRdt[Acl](privateIdentity) {
  override def invariants(
      hash: Hash,
      delta: BftDelta[Acl],
      prefixHashDag: HashDag[BftDelta[Acl], Acl]
  ): Boolean = {
    super.invariants(hash, delta, prefixHashDag) // delta is either root, or transitive child of root
    // either removal or delegation
    && delta.state.removed.isEmpty || delta.state.read.isEmpty && delta.state.write.isEmpty && delta.state.admins.isEmpty
    && delegationValid(
      delta.author,
      delta.state,
      cache(delta.parents).getOrElse(reconstructor(delta.parents, prefixHashDag))
    )
  }

  private def delegationValid(author: PublicIdentity, delta: Acl, prefix: Acl): Boolean = {
    val prefixReadPermOfAuthor  = prefix.read.getOrElse(author, PermissionTree.empty)
    val prefixWritePermOfAuthor = prefix.write.getOrElse(author, PermissionTree.empty)
    // Users that are removed in prefix are not allowed to perform any actions
    !prefix.removed.contains(author)
    // No delegations should happen to an already removed user and all delegations should come from a user that has
    // sufficient permissions.
    && prefix.read.forall { (delegatee, readPerm) =>
      !prefix.removed.contains(delegatee) && readPerm <= prefixReadPermOfAuthor
    }
    && prefix.write.forall { (delegatee, writePerm) =>
      !prefix.removed.contains(delegatee) && writePerm <= prefixWritePermOfAuthor
    }
    // Cannot grant admin rights to removed user
    && prefix.admins.forall { admin => !prefix.removed.contains(admin) }
    // Author is admin if delegating admin rights
    && (delta.admins.isEmpty || prefix.admins.contains(author))
  }
}

object AclRdt {
  given JsonValueCodec[BftDelta[Acl]] = {
    import lofi_acl.sync.JsoniterCodecs.given
    JsonCodecMaker.make(
      CodecMakerConfig.withAllowRecursiveTypes(true) // Required for PermissionTree
    )
  }

  given Encoder[BftDelta[Acl]] = Encoder.fromJsoniter

  given StateReconstructor[BftDelta[Acl], Acl] =
    (prefix: Set[Hash], hashDag: HashDag[BftDelta[Acl], Acl]) => HashDag.reduce(hashDag, prefix)
}
