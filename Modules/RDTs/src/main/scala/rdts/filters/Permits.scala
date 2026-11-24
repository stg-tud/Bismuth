package rdts.filters

import rdts.base.Bottom
import rdts.datatypes.{DotRemoveMap, LastWriterWins}
import rdts.filters.Permission.*
import rdts.time.{ArrayRanges, Dots}

import scala.compiletime.{constValue, erasedValue, summonAll}
import scala.deriving.Mirror

/** Checks whether deltas only touch parts permitted by a [[PermissionTree]]. */
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

  def ofTerminalValue[T: Bottom]: Permits[T] = PermitsDerivation.TerminalPermits[T]()

  // From https://blog.philipp-martini.de/blog/magic-mirror-scala3/
  inline def getElementNames[A <: Tuple]: List[String] = inline erasedValue[A] match {
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

  val terminalDotsPermits: Permits[Dots] = PermitsDerivation.TerminalPermits[Dots]()

  class DotsPermitsDeep extends Permits[Dots] {
    protected def permRangeToRange(dotPermissions: Map[String, PermissionTree]): ArrayRanges =
      ArrayRanges.from(
        dotPermissions.flatMap { (timeAsString, perm) =>
          if perm.isEmpty
          then None
          else Some(java.lang.Long.parseUnsignedLong(timeAsString))
        }
      )

    override def isAllowed(delta: Dots, permissionTree: PermissionTree): Boolean = permissionTree match
        case PermissionTree(ALLOW, _)                              => true
        case PermissionTree(PARTIAL, children) if children.isEmpty => delta.isEmpty
        case PermissionTree(PARTIAL, perms)                        =>
          delta.internal.forall((uid, ranges) =>
            perms.get(uid.delegate) match
                case None                                          => false
                case Some(PermissionTree(ALLOW, _))                => true
                case Some(PermissionTree(PARTIAL, dotPermissions)) => ranges <= permRangeToRange(dotPermissions)
          )

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      permissionTree.children.foreach { (_, childPerm) =>
        childPerm.children.foreach { (key, _) =>
          try {
            val _ = java.lang.Long.parseUnsignedLong(key)
          } catch {
            case e: NumberFormatException => throw InvalidPathException(key :: Nil)
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

  val dotsPermitsDeep: Permits[Dots] = DotsPermitsDeep()

  class OptionPermits[T](using inner: Permits[T]) extends Permits[Option[T]] {
    override def isAllowed(delta: Option[T], permissionTree: PermissionTree): Boolean = delta match {
      case None        => true // None is bottom, thus it must be allowed
      case Some(value) => inner.isAllowed(value, permissionTree)
    }

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.nonEmpty then
          inner.validatePermissionTree(permissionTree)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      inner.minimizePermissionTree(permissionTree)
  }

  given optionPermits[T: Permits]: Permits[Option[T]] = OptionPermits[T]()

  class TerminalSetPermits[T] extends Permits[Set[T]] {
    override def isAllowed(delta: Set[T], permissionTree: PermissionTree): Boolean = permissionTree match {
      case PermissionTree(ALLOW, _)                                            => true
      case PermissionTree(PARTIAL, entryPermission) if entryPermission.isEmpty => delta.isEmpty
      case PermissionTree(PARTIAL, entryPermission)                            =>
        throw IllegalArgumentException("Non-terminal rule used in terminal filter")
    }

    override def validatePermissionTree(permission: PermissionTree): Unit =
      if permission.children.nonEmpty then throw InvalidPathException(permission.children.keys.head :: Nil)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = permissionTree
  }

  def terminalSetPermits[T]: Permits[Set[T]] = TerminalSetPermits[T]()

  class SetPermits[T](using inner: Permits[T]) extends Permits[Set[T]] {
    override def isAllowed(delta: Set[T], permissionTree: PermissionTree): Boolean = permissionTree match
        case PermissionTree(ALLOW, _)                                              => true
        case PermissionTree(PARTIAL, entryPermissions) if entryPermissions.isEmpty => delta.isEmpty
        case PermissionTree(PARTIAL, entryPermissions)                             =>
          // TODO: Maybe add support for named child filters
          require(entryPermissions.size == 1, "Only * rules supported in Set filter")
          entryPermissions.get("*") match
              case Some(entryPermission) => delta.forall(entry => inner.isAllowed(entry, entryPermission))
              case None                  => ???

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.isEmpty
      then return
      else if permissionTree.children.size > 1
      then throw InvalidPathException(List.empty)
      else
          permissionTree.children.get("*") match
              case Some(entryPermission) => inner.validatePermissionTree(entryPermission)
              case None                  => throw InvalidPathException(List(permissionTree.children.keys.head))

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      PermissionTree(
        permission = permissionTree.permission,
        children = permissionTree.children.map((label, child) => label -> inner.minimizePermissionTree(child))
      )
  }

  given setPermits[T: Permits]: Permits[Set[T]] = SetPermits[T]()

  class MapPermits[K: KeyAsString, V](using inner: Permits[V]) extends Permits[Map[K, V]] {
    override def isAllowed(delta: Map[K, V], permissionTree: PermissionTree): Boolean =
      permissionTree match
          case PermissionTree(ALLOW, _)                       => true
          case PermissionTree(PARTIAL, mapOfEntryPermissions) =>
            val wildcard = permissionTree.children.get("*")
            delta.forall { case key -> value =>
              mapOfEntryPermissions.get(KeyAsString[K].encode(key)) match
                  case None                  => wildcard.exists(perm => inner.isAllowed(value, perm))
                  case Some(entryPermission) => inner.isAllowed(value, entryPermission)
            }

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      permissionTree.children.foreach {
        case keyPath -> pt =>
          try
              inner.validatePermissionTree(pt)
          catch
              case e @ InvalidPathException(subPath) => InvalidPathException(keyPath :: subPath)
      }

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
        val minimized = PermissionTree(
          permission = permissionTree.permission,
          children = permissionTree.children.map((label, child) => label -> inner.minimizePermissionTree(child))
        )

        if minimized.children.contains("*")
        then PermissionTree.lattice.normalizeWildcards(minimized)
        else minimized
  }

  given mapPermits[K: KeyAsString, V: Permits]: Permits[Map[K, V]] = MapPermits[K, V]()

  // TODO: this uses Bottom[LWW] which is not really well defined probably better to switch to a different datastructure
  class LwwPermits[V: Bottom](using inner: Permits[V]) extends Permits[LastWriterWins[V]] {
    override def isAllowed(delta: LastWriterWins[V], permissionTree: PermissionTree): Boolean = permissionTree match
        case PermissionTree(ALLOW, _)                                        => true
        case PermissionTree(PARTIAL, recursivePerm) if recursivePerm.isEmpty => LastWriterWins.bottom[V].isEmpty(delta)
        case PermissionTree(PARTIAL, _) => inner.isAllowed(delta.payload, permissionTree)

    override def validatePermissionTree(permissionTree: PermissionTree): Unit = permissionTree match
        case PermissionTree(ALLOW, _)   =>
        case PermissionTree(PARTIAL, _) => inner.validatePermissionTree(permissionTree)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      inner.minimizePermissionTree(permissionTree)
  }

  given lwwPermits[V: {Permits, Bottom}]: Permits[LastWriterWins[V]] = LwwPermits[V]()

  class TerminalLwwPermits[V: Bottom] extends Permits[LastWriterWins[V]] {
    override def isAllowed(delta: LastWriterWins[V], permissionTree: PermissionTree): Boolean = permissionTree match
        case PermissionTree(ALLOW, _)                 => true
        case PermissionTree(PARTIAL, valuePermission) =>
          // This is actually never reached, if using normalized permission trees
          require(valuePermission.isEmpty)
          LastWriterWins.bottom[V].isEmpty(delta)

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.nonEmpty then throw InvalidPathException(permissionTree.children.keys.head :: Nil)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = permissionTree
  }

  def terminalLwwPermits[V: Bottom]: Permits[LastWriterWins[V]] = TerminalLwwPermits[V]()

  given dotRemoveMapPermits[V: Permits]: Permits[DotRemoveMap[V]] = {
    given Permits[Dots] = terminalDotsPermits
    Permits.derived
  }
}
