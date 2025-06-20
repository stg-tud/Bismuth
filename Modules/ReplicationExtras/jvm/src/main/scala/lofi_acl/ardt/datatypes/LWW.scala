package lofi_acl.ardt.datatypes

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.access.*
import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins
import rdts.time.CausalTime

type LWW[V] = LastWriterWins[V]

object LWW {
  given recursiveFilter[V: {Filter}]: Filter[LWW[V]] with
    override def filter(delta: LWW[V], permission: PermissionTree): LWW[V] = permission match
      case PermissionTree(ALLOW, _)   => delta
      case PermissionTree(PARTIAL, _) => delta.copy(payload = Filter[V].filter(delta.read, permission))

    override def validatePermissionTree(permissionTree: PermissionTree): Unit = permissionTree match
      case PermissionTree(ALLOW, _)   =>
      case PermissionTree(PARTIAL, _) => Filter[V].validatePermissionTree(permissionTree)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      Filter[V].minimizePermissionTree(permissionTree)

  given terminalFilter[V: Bottom]: Filter[LWW[V]] with
    override def filter(delta: LWW[V], permission: PermissionTree): LWW[V] = permission match
      case PermissionTree(ALLOW, _)                 => delta
      case PermissionTree(PARTIAL, valuePermission) =>
        // This is actually never reached, if using normalized permission trees
        require(valuePermission.isEmpty)
        LastWriterWins.bottom[V].empty

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.nonEmpty then throw InvalidPathException(permissionTree.children.keys.head :: Nil)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = permissionTree

  private given causalTimeDeltaSurgeon: DeltaSurgeon[CausalTime] = {
    given codec: JsonValueCodec[CausalTime] = JsonCodecMaker.make[CausalTime]
    DeltaSurgeon.ofTerminalValue
  }
  given deltaSurgeon[V: {Bottom, DeltaSurgeon}]: DeltaSurgeon[LWW[V]] = DeltaSurgeon.derived
}
