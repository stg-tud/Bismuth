package rdts.filters

import rdts.base.Bottom
import rdts.filters.Permission.{ALLOW, PARTIAL}
import rdts.filters.PermissionTree.allow

import scala.deriving.Mirror

object PermitsDerivation {

  abstract class AlgebraicPermits[T](
      elementLabels: Map[String, Int],
      elementPermits: IArray[Permits[Any]],
  ) extends Permits[T] {

    /** Checks whether all children labels are the field/type names of this product and validates the permissions for the children.
      *
      * @param permissionTree The tree to check
      */
    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      permissionTree.children.foreach { case (factorLabel, childPermissionTree) =>
        elementLabels.get(factorLabel) match
            case Some(factorIdx) =>
              try
                elementPermits(factorIdx).validatePermissionTree(childPermissionTree)
              catch {
                case InvalidPathException(path) => throw InvalidPathException(factorLabel :: path)
              }
            case None if factorLabel == "*" =>
              try
                elementPermits.foreach(_.validatePermissionTree(childPermissionTree))
              catch {
                case InvalidPathException(labels) => throw InvalidPathException("*" :: labels)
              }
            case None => throw InvalidPathException(List(factorLabel))
      }

    // Assumes a valid permission tree
    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = {
      if permissionTree.permission == ALLOW then return allow

      val minimizedChildren = permissionTree.children.map { (label, subtree) =>
        val idx                            = elementLabels(label)
        val permits                        = elementPermits(idx)
        val minimizedChild: PermissionTree = permits.minimizePermissionTree(subtree)
        label -> minimizedChild
      }

      if minimizedChildren.size == elementPermits.size && minimizedChildren.forall(_._2.permission == ALLOW)
      then allow
      else PermissionTree(PARTIAL, minimizedChildren)
    }
  }

  class ProductTypePermits[T](
      productBottom: Bottom[T], // The bottom of the product (derivable as the product of bottoms)
      factorLabels: Array[String],
      factorBottoms: IArray[Bottom[Any]],  // The Bottom TypeClass instance for each factor
      factorPermits: IArray[Permits[Any]]  // The Permits TypeClass instance for each factor
  ) extends AlgebraicPermits[T](factorLabels.zipWithIndex.toMap, factorPermits):
      override def isAllowed(delta: T, permissionTree: PermissionTree): Boolean = {
        if permissionTree.permission == ALLOW then return true
        if permissionTree.children.isEmpty then return productBottom.isEmpty(delta)
        val product = delta.asInstanceOf[Product]
        permissionTree.children.get("*") match {
          case None => factorPermits.indices.forall(idx =>
                val element = product.productElement(idx)
                factorBottoms(idx).isEmpty(element) || (permissionTree.children.get(factorLabels(idx)) match {
                  case Some(childPermission) => factorPermits(idx).isAllowed(element, childPermission)
                  case None                  => false
                })
            )
          // Need to merge wildcard permission into specific permission
          case Some(wildcard) => factorLabels.indices.forall(idx =>
                val element = product.productElement(idx)
                factorBottoms(idx).isEmpty(element) || (permissionTree.children.get(factorLabels(idx)) match {
                  case Some(child) => factorPermits(idx).isAllowed(element, child.merge(wildcard))
                  case None        => factorPermits(idx).isAllowed(element, wildcard)
                })
            )
        }
      }

  class SumTypePermits[T](
      sm: Mirror.SumOf[T],
      bottom: Bottom[T],                    // The bottom of the sum
      elementNames: Array[String],          // The names of the types
      elementPermits: IArray[Permits[Any]]  // The Permits TypeClass instance for each element
  ) extends AlgebraicPermits[T](elementNames.zipWithIndex.toMap, elementPermits):
      override def isAllowed(delta: T, permissionTree: PermissionTree): Boolean = permissionTree match {
        case PermissionTree(ALLOW, _)                              => true
        case PermissionTree(PARTIAL, children) if children.isEmpty => bottom.isEmpty(delta)
        case PermissionTree(PARTIAL, children)                     =>
          val ordinal     = sm.ordinal(delta)
          val elementName = elementNames(ordinal)
          val wildcard    = children.getOrElse("*", PermissionTree.empty)
          children.getOrElse(elementName, PermissionTree.empty).merge(wildcard) match
              case PermissionTree(ALLOW, _)                              => true
              case PermissionTree(PARTIAL, children) if children.isEmpty => bottom.isEmpty(delta)
              case childPerm @ PermissionTree(PARTIAL, children)         =>
                elementPermits(ordinal).isAllowed(delta, childPerm)
      }

  class TerminalPermits[T: Bottom] extends Permits[T]:
      override def isAllowed(delta: T, permissionTree: PermissionTree): Boolean =
        permissionTree.permission == ALLOW || Bottom[T].isEmpty(delta)
      override def validatePermissionTree(permissionTree: PermissionTree): Unit =
        require(permissionTree.children.isEmpty)
      override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
        permissionTree match
            case PermissionTree(ALLOW, _)   => PermissionTree.allow
            case PermissionTree(PARTIAL, _) => PermissionTree.empty
}
