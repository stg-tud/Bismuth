package lofi_acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.AclRdt.given
import lofi_acl.bft.HashDag.Encoder
import rdts.base.{Bottom, DecoratedLattice, Lattice}
import rdts.filters.PermissionTree

case class Acl(
    read: Map[PublicIdentity, PermissionTree],
    write: Map[PublicIdentity, PermissionTree],
    removed: Set[PublicIdentity],
    admins: Set[PublicIdentity],
)

object Acl {
  given aclLattice: Lattice[Acl] = {
    import rdts.base.Lattice.mapLattice
    DecoratedLattice.filter(Lattice.derived[Acl])((base, other) =>
      if other.removed.isEmpty then base
      else
          base.copy(
            read = base.read.removedAll(other.removed),
            write = base.write.removedAll(other.removed),
            admins = base.admins.removedAll(other.removed)
          )
    )
  }

  given aclBottom: Bottom[Acl] = Bottom.derived
}

class AclRdt(privateIdentity: PrivateIdentity) extends BftSignedDeltaRdt[Acl](privateIdentity) {
  override def invariants(
      hash: Hash,
      delta: SignedDelta[Acl],
      prefixHashDag: HashDag[SignedDelta[Acl], Acl]
  ): Boolean = {
    super.invariants(hash, delta, prefixHashDag) // delta is either root, or transitive child of root
    // either removal or delegation
    && delta.rdt.removed.isEmpty || delta.rdt.read.isEmpty && delta.rdt.write.isEmpty && delta.rdt.admins.isEmpty
    && delegationValid(delta.author, delta.rdt, reconstructor(delta.parents, prefixHashDag))
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
  given JsonValueCodec[SignedDelta[Acl]] = {
    import lofi_acl.sync.JsoniterCodecs.given
    JsonCodecMaker.make(
      CodecMakerConfig.withAllowRecursiveTypes(true) // Required for PermissionTree
    )
  }

  given Encoder[SignedDelta[Acl]] = Encoder.fromJsoniter

  given StateReconstructor[SignedDelta[Acl], Acl] =
    (prefix: Set[Hash], hashDag: HashDag[SignedDelta[Acl], Acl]) => HashDag.reduce(hashDag, prefix)
}
