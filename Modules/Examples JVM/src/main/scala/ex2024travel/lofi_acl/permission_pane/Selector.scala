package ex2024travel.lofi_acl.permission_pane

import ex2024travel.lofi_acl.permission_pane.Selector.{FixedSelector, leafSelector}
import rdts.base.Bottom
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}
import rdts.filters.KeyAsString

import scala.compiletime.{constValueTuple, erasedValue, summonAll}
import scala.deriving.Mirror

trait Selector[ARDT](
    val writePreSelected: Map[String, Boolean] = Map.empty,
    val readPreSelected: Map[String, Boolean] = Map.empty,
    val writeDelegatable: Map[String, Boolean] = Map.empty,
    val readDelegatable: Map[String, Boolean] = Map.empty,
) {
  def children: Map[String, Selector[?]]
}

object Selector {
  inline def apply[T](using selector: Selector[T]): Selector[T] = selector

  def leafSelector[T]: Selector[T] = FixedSelector[T](Map.empty)

  class FixedSelector[T](constantChildren: Map[String, Selector[?]]) extends Selector[T] {
    override def children: Map[String, Selector[?]] = constantChildren
  }
}

trait SelectorFactory[ARDT] {
  def create(
      ardt: ARDT,
      // writeSelectable: PermissionTree = allow,
      // readSelectable: PermissionTree = allow,
      // existingWrite: PermissionTree = empty,
      // existingRead: PermissionTree = empty,
  ): Selector[ARDT]
}

object SelectorFactory {
  inline def apply[T](using selectorFactory: SelectorFactory[T]): SelectorFactory[T] = selectorFactory

  inline def derived[ARDT <: Product](using pm: Mirror.ProductOf[ARDT]): SelectorFactory[ARDT] = (ardt: ARDT) => {
    val childLabels    = constValueTuple[pm.MirroredElemLabels]
    val childSelectors = summonAll[Tuple.Map[pm.MirroredElemTypes, SelectorFactory]]
    FixedSelector[ARDT](processChildren[ARDT, pm.MirroredElemTypes](ardt, 0, childLabels, childSelectors))
  }

  inline def processChildren[ARDT <: Product, T <: Tuple](
      ardt: ARDT,
      idx: Int,
      childLabels: Tuple,
      childSelectors: Tuple,
      // writeSelectable: PermissionTree,
      // readSelectable: PermissionTree,
      // existingRead: PermissionTree,
      // existingWrite: PermissionTree
  ): Map[String, Selector[?]] = inline erasedValue[T] match
      case _: EmptyTuple     => Map.empty
      case _: (head *: tail) =>
        val childLabel = childLabels.productElement(idx).asInstanceOf[String]
        val factory    = childSelectors.productElement(idx).asInstanceOf[SelectorFactory[head]]
        val selector   = factory.create(
          ardt.productElement(idx).asInstanceOf[head],
          // writeSelectable.descend(childLabel),
          // readSelectable.descend(childLabel),
          // existingWrite.descend(childLabel),
          // existingRead.descend(childLabel),
        )
        processChildren[ARDT, tail](ardt, idx + 1, childLabels, childSelectors) + (childLabel -> selector)

  def leafSelectorFactory[T]: SelectorFactory[T] = (ardt: T) => leafSelector

  given SelectorFactory[String] = leafSelectorFactory

  class BoxedValueSelectorFactory[Box, V: SelectorFactory](extractor: Box => V) extends SelectorFactory[Box] {
    override def create(ardt: Box): Selector[Box] = new Selector[Box]() {
      override def children: Map[String, Selector[?]] = SelectorFactory[V].create(extractor(ardt)).children
    }
  }

  given OptionSelectorFactory[T: {SelectorFactory, Bottom}]: SelectorFactory[Option[T]] =
    BoxedValueSelectorFactory[Option[T], T](opt => opt.getOrElse(Bottom[T].empty))

  given ORMapSelectorFactory[K: KeyAsString, V: {SelectorFactory, Bottom}]: SelectorFactory[ObserveRemoveMap[K, V]] with
      override def create(ardt: ObserveRemoveMap[K, V]): Selector[ObserveRemoveMap[K, V]] = {
        new Selector[ObserveRemoveMap[K, V]]() {
          override def children: Map[String, Selector[V]] = {
            val wildcard = SelectorFactory[V].create(Bottom[V].empty)
            ((if wildcard.children.isEmpty then Seq.empty else Seq("*" -> wildcard)) ++ ardt.entries.map((k, v) =>
                val keyString = KeyAsString[K].encode(k)
                keyString -> SelectorFactory[V].create(v)
            )).toMap
          }
        }
      }

  given LwwSelectorFactory[V: SelectorFactory]: SelectorFactory[LastWriterWins[V]] =
    BoxedValueSelectorFactory[LastWriterWins[V], V](lww => lww.read)
}
