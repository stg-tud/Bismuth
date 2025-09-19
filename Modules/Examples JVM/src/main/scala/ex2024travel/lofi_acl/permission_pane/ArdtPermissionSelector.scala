package ex2024travel.lofi_acl.permission_pane

trait DisplayablePermission {
  val uiText: String
  val children: Map[String, DisplayablePermission]
}

class ArdtPermissionSelector[ARDT](
    val label: String,
    val children: Map[String, ArdtPermissionSelector[?]] = Map.empty
) {
  val uiText: String      = label
  def isWildcard: Boolean = label == "*"

  val hasChildren: Boolean = children.isEmpty

  // TODO: get disabled state from ACL
  // - disable
  // TODO: get selection from previous state
  // - pre select previous permissions
}

object ArdtPermissionSelector {
  // TODO: derive for product and sum types

  // TODO: ORMap

  // TODO: LWWMap
}
