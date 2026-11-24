package rdts.filters

import rdts.base.Bottom

import scala.compiletime.{constValue, erasedValue, summonAll}
import scala.deriving.Mirror

/** Checks deltas against a [[PermissionTree]] without being able to prune them.
  * [[Filter]] extends this with the ability to remove the parts that are not permitted.
  */
trait Permits[T] {

  def isAllowed(delta: T, permissionTree: PermissionTree): Boolean

  /** Checks whether the permission tree is valid.
    * <li> Not DENY & ALLOW on the same level
    * <li> All fields exist
    *
    * @param permissionTree The tree to check
    * @throws InvalidPathException if the tree references a path that does not exist.
    */
  def validatePermissionTree(permissionTree: PermissionTree): Unit

  def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree
}

object Permits {
  inline def apply[T](using permits: Permits[T]): Permits[T] = permits

  /** Every [[Filter]] can be used as [[Permits]]. Needed because instances defined in the [[Filter]] companion are not
    * in the implicit scope of `Permits[T]`.
    */
  given fromFilter[T](using filter: Filter[T]): Permits[T] = filter

  def ofTerminalValue[T: Bottom]: Permits[T] = PermitsDerivation.TerminalPermits[T]()

  // From https://blog.philipp-martini.de/blog/magic-mirror-scala3/
  private[filters] inline def getElementNames[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].toString :: getElementNames[ts]
  }

  inline def derived[T](using m: Mirror.Of[T], bottom: Bottom[T]): Permits[T] = {
    val elementBottoms = summonAll[Tuple.Map[m.MirroredElemTypes, Bottom]].toIArray.map(_.asInstanceOf[Bottom[Any]])
    val elementPermits =
      summonAll[Tuple.Map[m.MirroredElemTypes, Permits]].toIArray.map(_.asInstanceOf[Permits[Any]])
    val elementNames = getElementNames[m.MirroredElemLabels]
    require(elementNames.toSet.size == elementNames.length) // Ensure uniqueness of element names
    inline m match
        case _: Mirror.ProductOf[T] =>
          PermitsDerivation.ProductTypePermits[T](bottom, elementNames.toArray, elementBottoms, elementPermits)
        case sumMirror: Mirror.SumOf[T] =>
          PermitsDerivation.SumTypePermits[T](sumMirror, bottom, elementNames.toArray, elementPermits)
  }
}
