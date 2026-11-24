package ex2026filtering

import ex2026filtering.FilterDerivation.TerminalFilter
import rdts.base.Bottom
import rdts.datatypes.{DotRemoveMap, LastWriterWins}
import rdts.filters.Permission.*
import rdts.filters.Permits.*
import rdts.filters.{KeyAsString, PermissionTree, Permits}
import rdts.time.Dots

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** A [[Permits]] that can additionally remove the parts of a delta that are not permitted. */
trait Filter[T] extends Permits[T] {
  def filter(delta: T, permission: PermissionTree): T
}

object Filter {
  inline def apply[T](using filter: Filter[T]): Filter[T] = filter

  def ofTerminalValue[T: Bottom]: Filter[T] = TerminalFilter[T]()

  inline def derived[T](using m: Mirror.Of[T], productBottom: Bottom[T]): Filter[T] = {
    // Element is either a factor of a product or an element in a sum
    val elementBottoms = summonAll[Tuple.Map[m.MirroredElemTypes, Bottom]].toIArray.map(_.asInstanceOf[Bottom[Any]])
    val elementFilters = summonAll[Tuple.Map[m.MirroredElemTypes, Filter]].toIArray.map(_.asInstanceOf[Filter[Any]])
    val elementNames   = Permits.getElementNames[m.MirroredElemLabels]
    require(elementNames.toSet.size == elementNames.length) // Ensure uniqueness of element names
    inline m match
        case prodMirror: Mirror.ProductOf[T] =>
          FilterDerivation.ProductTypeFilter[T](
            prodMirror,
            productBottom,
            elementNames.toArray,
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

  val terminalDotsFilter: Filter[Dots] = TerminalFilter[Dots]()

  val dotsFilterDeep: Filter[Dots] = new DotsPermitsDeep with Filter[Dots] {
    override def filter(delta: Dots, permission: PermissionTree): Dots =
      permission match
          case PermissionTree(ALLOW, _)                              => delta
          case PermissionTree(PARTIAL, children) if children.isEmpty => Dots.empty
          case PermissionTree(PARTIAL, perms)                        =>
            Dots(delta.internal.flatMap((uid, ranges) =>
              perms.get(uid.delegate) match
                  case Some(PermissionTree(ALLOW, _))                => Some(uid -> ranges)
                  case Some(PermissionTree(PARTIAL, dotPermissions)) =>
                    Some(uid -> ranges.intersect(permRangeToRange(dotPermissions)))
                  case None => None
            ))
  }

  given optionFilter[T: Filter]: Filter[Option[T]] = new OptionPermits[T] with Filter[Option[T]] {
    override def filter(delta: Option[T], permission: PermissionTree): Option[T] =
      delta match
          case None        => delta
          case Some(value) => permission match
                case PermissionTree(ALLOW, _)          => delta
                case PermissionTree(PARTIAL, children) =>
                  // NOTE: Some(bottom) is not None.
                  // NOTE: PermissionTree(PARTIAL, Map("a" -> allow)) on Option[T] keeps the field a of T.
                  Some(Filter[T].filter(value, permission))
  }

  def terminalSetFilter[T]: Filter[Set[T]] = new TerminalSetPermits[T] with Filter[Set[T]] {
    override def filter(delta: Set[T], permission: PermissionTree): Set[T] = permission match
        case PermissionTree(ALLOW, _)                                              => delta
        case PermissionTree(PARTIAL, entryPermissions) if entryPermissions.isEmpty => Set.empty
        case PermissionTree(PARTIAL, entryPermissions)                             =>
          throw IllegalArgumentException("Non-terminal rule used in terminal filter")
  }

  given setFilter[T: Filter]: Filter[Set[T]] = new SetPermits[T] with Filter[Set[T]] {
    override def filter(delta: Set[T], permission: PermissionTree): Set[T] = permission match
        case PermissionTree(ALLOW, _)                                              => delta
        case PermissionTree(PARTIAL, entryPermissions) if entryPermissions.isEmpty => Set.empty
        case PermissionTree(PARTIAL, entryPermissions)                             =>
          // TODO: Maybe add support for named child filters
          require(entryPermissions.size == 1, "Only * rules supported in Set filter")
          entryPermissions.get("*") match
              case Some(entryPermission) => delta.map(entry => Filter[T].filter(entry, entryPermission))
              case None                  => ???
  }

  given mapFilter[K: KeyAsString, V: Filter]: Filter[Map[K, V]] = new MapPermits[K, V] with Filter[Map[K, V]] {
    override def filter(delta: Map[K, V], permission: PermissionTree): Map[K, V] = permission match
        case PermissionTree(ALLOW, _)                       => delta
        case PermissionTree(PARTIAL, mapOfEntryPermissions) =>
          val wildcard = permission.children.get("*")
          delta.flatMap { case key -> value =>
            mapOfEntryPermissions.get(KeyAsString[K].encode(key)) match
                case None                  => wildcard.map(perm => key -> Filter[V].filter(value, perm))
                case Some(entryPermission) => Some(key -> Filter[V].filter(value, entryPermission))
          }
  }

  // TODO: this uses Bottom[LWW] which is not really well defined probably better to switch to a different datastructure
  given lwwFilter[V: {Filter, Bottom}]: Filter[LastWriterWins[V]] =
    new LwwPermits[V] with Filter[LastWriterWins[V]] {
      override def filter(delta: LastWriterWins[V], permission: PermissionTree): LastWriterWins[V] = permission match
          case PermissionTree(ALLOW, _)                                        => delta
          case PermissionTree(PARTIAL, recursivePerm) if recursivePerm.isEmpty => LastWriterWins.bottom[V].empty
          case PermissionTree(PARTIAL, _) => delta.copy(payload = Filter[V].filter(delta.read, permission))
    }

  def terminalLwwFilter[V: Bottom]: Filter[LastWriterWins[V]] =
    new TerminalLwwPermits[V] with Filter[LastWriterWins[V]] {
      override def filter(delta: LastWriterWins[V], permission: PermissionTree): LastWriterWins[V] = permission match
          case PermissionTree(ALLOW, _)                 => delta
          case PermissionTree(PARTIAL, valuePermission) =>
            // This is actually never reached, if using normalized permission trees
            require(valuePermission.isEmpty)
            LastWriterWins.bottom[V].empty
    }

  given dotRemoveMapFilter[V: Filter]: Filter[DotRemoveMap[V]] = {
    given Filter[Dots] = terminalDotsFilter
    Filter.derived
  }
}
