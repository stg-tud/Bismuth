package ex2026filtering

import rdts.base.Bottom
import rdts.filters.Permission.{ALLOW, PARTIAL}
import rdts.filters.PermissionTree
import rdts.filters.PermitsDerivation.{ProductTypePermits, SumTypePermits, TerminalPermits}

import scala.annotation.unused
import scala.deriving.Mirror

/** Extends the derived [[rdts.filters.Permits]] instances from [[rdts.filters.PermitsDerivation]] with [[Filter.filter]]. */
object FilterDerivation {

  class ProductTypeFilter[T](
      pm: Mirror.ProductOf[T],
      productBottom: Bottom[T], // The bottom of the product (derivable as the product of bottoms)
      factorLabels: Array[String],
      factorBottoms: IArray[Bottom[Any]], // The Bottom TypeClass instance for each factor
      factorFilters: IArray[Filter[Any]]  // The Filter TypeClass instance for each factor
  ) extends ProductTypePermits[T](productBottom, factorLabels, factorBottoms, factorFilters), Filter[T]:
      override def filter(delta: T, permissionTree: PermissionTree): T =
        permissionTree match
            case PermissionTree(ALLOW, _)                              => delta
            case PermissionTree(PARTIAL, children) if children.isEmpty => productBottom.empty
            case PermissionTree(PARTIAL, children)                     =>
              // Apply filters to factors, if rule for factor is specified.
              // Otherwise use bottom for factor
              val filteredProduct = filterProduct(delta.asInstanceOf[Product], permissionTree)
              pm.fromProduct(filteredProduct)

      private def filterProduct(product: Product, permissionTree: PermissionTree): Product =
        // We assume that permission is PARTIAL and children nonempty
        permissionTree.children.get("*") match
            case None => // No wildcard
              new Product:
                  def canEqual(that: Any): Boolean          = false
                  def productArity: Int                     = factorBottoms.length
                  def productElement(factorIndex: Int): Any =
                      val label  = factorLabels(factorIndex)
                      val factor = product.productElement(factorIndex)
                      permissionTree.children.get(label) match {
                        case Some(factorPermission) => factorFilters(factorIndex).filter(factor, factorPermission)
                        case None                   => factorBottoms(factorIndex).empty
                      }
            case Some(wildcard) => // Wildcard
              new Product:
                  def canEqual(that: Any): Boolean          = false
                  def productArity: Int                     = factorBottoms.length
                  def productElement(factorIndex: Int): Any =
                      val label       = factorLabels(factorIndex)
                      val factor      = product.productElement(factorIndex)
                      val permissions = permissionTree.children.getOrElse(label, wildcard)
                      factorFilters(factorIndex).filter(factor, permissions)

  class SumTypeFilter[T](
      sm: Mirror.SumOf[T],
      bottom: Bottom[T],                           // The bottom of the sum
      elementNames: Array[String],                 // The names of the types
      @unused elementBottoms: IArray[Bottom[Any]], // The Bottom TypeClass instance for each element
      elementFilters: IArray[Filter[Any]]          // The Filter TypeClass instance for each element
  ) extends SumTypePermits[T](sm, bottom, elementNames, elementFilters), Filter[T]:
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

  class TerminalFilter[T: Bottom] extends TerminalPermits[T], Filter[T]:
      override def filter(delta: T, permission: PermissionTree): T =
        permission match
            case PermissionTree(ALLOW, _)   => delta
            case PermissionTree(PARTIAL, _) => Bottom[T].empty
}
