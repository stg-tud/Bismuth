package rdts.filters

import rdts.base.Bottom
import rdts.filters.Permission.{ALLOW, PARTIAL}
import rdts.filters.PermissionTree.allow
import rdts.time.{ArrayRanges, Dots}

import scala.annotation.unused
import scala.deriving.Mirror

object FilterDerivation {

  abstract protected class AlgebraicFilter[T](
      elementLabels: Map[String, Int],
      elementFilters: IArray[Filter[Any]],
  ) extends Filter[T] {

    /** Checks whether all children labels are the field/type names of this product and validates the filters for the children.
      *
      * @param permissionTree The tree to check
      */
    override def validatePermissionTree(permissionTree: PermissionTree): Unit = {
      permissionTree.children.foreach { case (factorLabel, childPermissionTree) =>
        elementLabels.get(factorLabel) match
          case Some(factorIdx) =>
            try
              elementFilters(factorIdx).validatePermissionTree(childPermissionTree)
            catch {
              case InvalidPathException(path) => throw InvalidPathException(factorLabel :: path)
            }
          case None if factorLabel == "*" =>
            try
              elementFilters.foreach(_.validatePermissionTree(childPermissionTree))
            catch {
              case InvalidPathException(labels) => throw InvalidPathException("*" :: labels)
            }
          case None => throw InvalidPathException(List(factorLabel))
      }
    }

    // Assumes a valid permission tree
    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = {
      if permissionTree.permission == ALLOW then return allow

      val minimizedChildren = permissionTree.children.map { (label, subtree) =>
        val idx                            = elementLabels(label)
        val filter                         = elementFilters(idx)
        val minimizedChild: PermissionTree = filter.minimizePermissionTree(subtree)
        label -> minimizedChild
      }

      if minimizedChildren.size == elementFilters.size && minimizedChildren.forall(_._2.permission == ALLOW)
      then allow
      else PermissionTree(PARTIAL, minimizedChildren)
    }
  }

  class ProductTypeFilter[T](
      pm: Mirror.ProductOf[T],
      productBottom: Bottom[T],           // The bottom of the product (derivable as the product of bottoms)
      factorLabels: Map[String, Int],     // Maps the factor label to the factor index
      factorBottoms: IArray[Bottom[Any]], // The Bottom TypeClass instance for each factor
      factorFilters: IArray[Filter[Any]]  // The Filter TypeClass instance for each factor
  ) extends AlgebraicFilter[T](factorLabels, factorFilters):
    override def filter(delta: T, permissionTree: PermissionTree): T = {
      permissionTree match
        case PermissionTree(ALLOW, _)                              => delta
        case PermissionTree(PARTIAL, children) if children.isEmpty => productBottom.empty
        case PermissionTree(PARTIAL, children)                     =>
          // Apply filters to factors, if rule for factor is specified.
          // Otherwise use bottom for factor
          val filteredProduct = filterProduct(delta.asInstanceOf[Product], permissionTree)
          pm.fromProduct(filteredProduct)
    }

    private def filterProduct(product: Product, permissionTree: PermissionTree): Product = {
      permissionTree.children.get("*") match
        case None =>
          val filteredFactors = permissionTree.children.map { case (factorName, permissionSubTree) =>
            // Assumes that permission tree is valid (i.e., factorName is a valid element)
            val factorIndex   = factorLabels(factorName)
            val factorOfDelta = product.productElement(factorIndex)
            factorIndex -> factorFilters(factorIndex).filter(factorOfDelta, permissionSubTree)
          }
          new Product:
            def canEqual(that: Any): Boolean = false
            def productArity: Int            = factorBottoms.length
            def productElement(i: Int): Any  = filteredFactors.getOrElse(i, factorBottoms(i).empty)
        case Some(wildcard) =>
          val filteredFactors = factorLabels.map { (label, factorIndex) =>
            val permissions   = permissionTree.children.getOrElse(label, wildcard)
            val factorOfDelta = product.productElement(factorIndex)
            factorIndex -> factorFilters(factorIndex).filter(factorOfDelta, permissions)
          }
          new Product:
            def canEqual(that: Any): Boolean = false
            def productArity: Int            = factorBottoms.length
            def productElement(i: Int): Any  = filteredFactors(i)
    }

  class SumTypeFilter[T](
      sm: Mirror.SumOf[T],
      bottom: Bottom[T],                           // The bottom of the sum
      elementNames: Array[String],                 // The names of the types
      @unused elementBottoms: IArray[Bottom[Any]], // The Bottom TypeClass instance for each element
      elementFilters: IArray[Filter[Any]]          // The Filter TypeClass instance for each element
  ) extends AlgebraicFilter[T](elementNames.zipWithIndex.toMap, elementFilters):
    override def filter(delta: T, permission: PermissionTree): T =
      permission match
        case PermissionTree(ALLOW, _)                              => delta
        case PermissionTree(PARTIAL, children) if children.isEmpty => bottom.empty
        case PermissionTree(PARTIAL, children)                     =>
          val ordinal     = sm.ordinal(delta)
          val elementName = elementNames(ordinal)
          children.getOrElse(elementName, children.getOrElse("*", PermissionTree.empty)) match
            case PermissionTree(ALLOW, _)                              => delta
            case PermissionTree(PARTIAL, children) if children.isEmpty => bottom.empty
            case childPerm @ PermissionTree(PARTIAL, children)         =>
              elementFilters(ordinal).filter(delta, childPerm).asInstanceOf[T]

  class TerminalFilter[T: Bottom] extends Filter[T]:
    override def filter(delta: T, permission: PermissionTree): T =
      permission match
        case PermissionTree(ALLOW, _)   => delta
        case PermissionTree(PARTIAL, _) => Bottom[T].empty
    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      require(permissionTree.children.isEmpty)
    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      permissionTree match
        case PermissionTree(ALLOW, _)   => PermissionTree.allow
        case PermissionTree(PARTIAL, _) => PermissionTree.empty

}
