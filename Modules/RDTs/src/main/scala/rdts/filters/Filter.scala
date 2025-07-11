package rdts.filters

import PermissionTree.allow
import Permission.*
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins
import rdts.filters.FilterDerivation.TerminalFilter
import rdts.time.{ArrayRanges, Dots}

import scala.annotation.unused
import scala.compiletime.{constValue, erasedValue, summonAll}
import scala.deriving.Mirror

trait Filter[T] {
  def filter(delta: T, permission: PermissionTree): T

  /** Checks whether the permission tree is valid.
    * <li> Not DENY & ALLOW on the same level
    * <li> All fields exist
    *
    * @param permissionTree The tree to check
    * @return Success(the validated permission tree) or a Failure(with the cause).
    */
  def validatePermissionTree(permissionTree: PermissionTree): Unit

  def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree

}

object Filter {
  inline def apply[T](using filter: Filter[T]): Filter[T] = filter

  def ofTerminalValue[T: Bottom]: Filter[T] = TerminalFilter[T]()

  // From https://blog.philipp-martini.de/blog/magic-mirror-scala3/
  private inline def getElementNames[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].toString :: getElementNames[ts]
  }

  inline def derived[T](using m: Mirror.Of[T], productBottom: Bottom[T]): Filter[T] = {
    // Element is either a factor of a product or an element in a sum
    val elementBottoms = summonAll[Tuple.Map[m.MirroredElemTypes, Bottom]].toIArray.map(_.asInstanceOf[Bottom[Any]])
    val elementFilters = summonAll[Tuple.Map[m.MirroredElemTypes, Filter]].toIArray.map(_.asInstanceOf[Filter[Any]])
    val elementNames   = getElementNames[m.MirroredElemLabels]
    require(elementNames.toSet.size == elementNames.length) // Ensure uniqueness of element names
    inline m match
      case prodMirror: Mirror.ProductOf[T] =>
        FilterDerivation.ProductTypeFilter[T](
          prodMirror,
          productBottom,
          elementNames.zipWithIndex.toMap,
          elementBottoms,
          elementFilters
        )
      case sumMirror: Mirror.SumOf[T] =>
        FilterDerivation.SumTypeFilter[T](
          sumMirror,
          productBottom,
          elementNames.toArray,
          elementBottoms,
          elementFilters
        )
  }

  given dotsFilter: Filter[Dots] with {
    override def filter(delta: Dots, permission: PermissionTree): Dots =
      permission match
        case PermissionTree(ALLOW, _)                              => delta
        case PermissionTree(PARTIAL, children) if children.isEmpty => Dots.empty
        case PermissionTree(PARTIAL, perms)                        =>
          Dots(delta.internal.flatMap((uid, ranges) =>
            perms.get(uid.delegate) match
              case Some(PermissionTree(ALLOW, _))                => Some(uid -> ranges)
              case Some(PermissionTree(PARTIAL, dotPermissions)) =>
                val allowedRanges = ArrayRanges.from(
                  dotPermissions.flatMap { (timeAsString, perm) =>
                    if perm.isEmpty
                    then None
                    else Some(java.lang.Long.parseUnsignedLong(timeAsString))
                  }
                )
                Some(uid -> ranges.intersect(allowedRanges))
              case None => None
          ))

    override def validatePermissionTree(permissionTree: PermissionTree): Unit = {
      permissionTree.children.foreach { (_, childPerm) =>
        childPerm.children.foreach { (key, value) =>
          try {
            val _ = java.lang.Long.parseUnsignedLong(key)
          } catch {
            case e: NumberFormatException => throw InvalidPathException(key :: Nil)
          }
        }
      }
    }

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      if permissionTree.permission == ALLOW then return PermissionTree.allow
      PermissionTree(
        PARTIAL,
        permissionTree.children.flatMap {
          case (uid, PermissionTree(ALLOW, _))                              => Some(uid -> PermissionTree.allow)
          case (uid, PermissionTree(PARTIAL, dotPerms)) if dotPerms.isEmpty => None
          case (uid, PermissionTree(PARTIAL, dotPerms))                     =>
            dotPerms.flatMap {
              case (time, PermissionTree(ALLOW, _))   => Some(time -> PermissionTree.allow)
              case (time, PermissionTree(PARTIAL, _)) => None
            }
        }
      )
  }

  given optionFilter[T: Filter]: Filter[Option[T]] with {
    override def filter(delta: Option[T], permission: PermissionTree): Option[T] =
      delta match
        case None        => delta
        case Some(value) => permission match
            case PermissionTree(ALLOW, _)          => delta
            case PermissionTree(PARTIAL, children) =>
              // NOTE: Some(bottom) is not None.
              // NOTE: PermissionTree(PARTIAL, Map("a" -> allow)) on Option[T] keeps the field a of T.
              Some(Filter[T].filter(value, permission))

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.nonEmpty then
        Filter[T].validatePermissionTree(permissionTree)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      Filter[T].minimizePermissionTree(permissionTree)
  }

  def terminalSetFilter[T]: Filter[Set[T]] = new Filter[Set[T]] {
    override def filter(delta: Set[T], permission: PermissionTree): Set[T] = permission match
      case PermissionTree(ALLOW, _)                                              => delta
      case PermissionTree(PARTIAL, entryPermissions) if entryPermissions.isEmpty => Set.empty
      case PermissionTree(PARTIAL, entryPermissions)                             =>
        throw IllegalArgumentException("Non-terminal rule used in terminal filter")

    override def validatePermissionTree(permission: PermissionTree): Unit =
      if permission.children.nonEmpty then throw InvalidPathException(permission.children.keys.head :: Nil)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = permissionTree
  }

  given setFilter[T: Filter]: Filter[Set[T]] with {
    override def filter(delta: Set[T], permission: PermissionTree): Set[T] = permission match
      case PermissionTree(ALLOW, _)                                              => delta
      case PermissionTree(PARTIAL, entryPermissions) if entryPermissions.isEmpty => Set.empty
      case PermissionTree(PARTIAL, entryPermissions)                             =>
        // TODO: Maybe add support for named child filters
        require(entryPermissions.size == 1, "Only * rules supported in Set filter")
        entryPermissions.get("*") match
          case Some(entryPermission) => delta.map(entry => Filter[T].filter(entry, entryPermission))
          case None                  => ???

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.isEmpty
      then return
      else if permissionTree.children.size > 1
      then throw InvalidPathException(List.empty)
      else
        permissionTree.children.get("*") match
          case Some(entryPermission) => Filter[T].validatePermissionTree(entryPermission)
          case None                  => throw InvalidPathException(List(permissionTree.children.keys.head))

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      PermissionTree(
        permission = permissionTree.permission,
        children = permissionTree.children.map((label, child) => label -> Filter[T].minimizePermissionTree(child))
      )
  }

  given mapFilter[K, V: Filter]: Filter[Map[K, V]] with {
    override def filter(delta: Map[K, V], permission: PermissionTree): Map[K, V] =
      permission match
        case PermissionTree(ALLOW, _)                       => delta
        case PermissionTree(PARTIAL, mapOfEntryPermissions) =>
          // TODO: Add MapLikeFilter typeclass that extracts this and makes equality check of key explicit
          delta.flatMap { case key -> value =>
            // TODO: Replace key.toString based lookup with typeclass based equality
            mapOfEntryPermissions.get(key.toString) match
              case None /* No rule for key -> discard entry */ => None
              case Some(entryPermission)                       => Some(key -> Filter[V].filter(value, entryPermission))
          }

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      permissionTree.children.foreach {
        case keyPath -> pt =>
          try {
            Filter[V].validatePermissionTree(pt)
          } catch
            case e @ InvalidPathException(subPath) => InvalidPathException(keyPath :: subPath)
      }

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      val minimized = PermissionTree(
        permission = permissionTree.permission,
        children = permissionTree.children.map((label, child) => label -> Filter[V].minimizePermissionTree(child))
      )

      if minimized.children.contains("*")
      then PermissionTree.lattice.normalizeWildcards(minimized)
      else minimized
  }

  given lwwFilter[V: {Filter}]: Filter[LastWriterWins[V]] with {
    override def filter(delta: LastWriterWins[V], permission: PermissionTree): LastWriterWins[V] = permission match
      case PermissionTree(ALLOW, _)   => delta
      case PermissionTree(PARTIAL, _) => delta.copy(payload = Filter[V].filter(delta.read, permission))

    override def validatePermissionTree(permissionTree: PermissionTree): Unit = permissionTree match
      case PermissionTree(ALLOW, _)   =>
      case PermissionTree(PARTIAL, _) => Filter[V].validatePermissionTree(permissionTree)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      Filter[V].minimizePermissionTree(permissionTree)
  }

  def terminalLwwFilter[V: Bottom]: Filter[LastWriterWins[V]] = new {
    override def filter(delta: LastWriterWins[V], permission: PermissionTree): LastWriterWins[V] = permission match
      case PermissionTree(ALLOW, _)                 => delta
      case PermissionTree(PARTIAL, valuePermission) =>
        // This is actually never reached, if using normalized permission trees
        require(valuePermission.isEmpty)
        LastWriterWins.bottom[V].empty

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.nonEmpty then throw InvalidPathException(permissionTree.children.keys.head :: Nil)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = permissionTree
  }
}
