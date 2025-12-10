package rdts.datatypes

import rdts.time.Dot
import rdts.base.LocalUid
import rdts.time.Dots
import rdts.base.Lattice
import scala.collection.mutable as mutable

case class ReplicatedTree[A](
    elements: Map[Dot, ReplicatedTree.Node[A]],
    removed: Dots = Dots.empty
) {
  type Delta = ReplicatedTree[A]

  lazy val observed = elements.keySet.foldLeft(removed)((acc, dot) => acc `union` Dots.single(dot))

  lazy val compact = elements.filter((k, _) => !removed.contains(k))

  def isEmpty: Boolean = compact.isEmpty

  def size: Int = compact.size

  def nodes: Iterable[ReplicatedTree.Node[A]] = compact.values

  def node(dot: Dot): Option[ReplicatedTree.Node[A]] = compact.get(dot)

  def parent(dot: Dot): Option[Dot] = compact.get(dot).map(_.parent)

  def root: Option[ReplicatedTree.Node[A]] = {
    val c = children(ReplicatedTree.rootDot)
    assert(c.size <= 1)
    c.headOption
  }

  def children(dot: Dot): Iterable[ReplicatedTree.Node[A]] = {
    compact.values.filter(n => n.parent == dot)
  }

  def insert(parent: Dot, value: A)(using LocalUid): Delta = {
    insertWith(parent, _ => value)
  }

  def insertWith(parent: Dot, value: Dot => A)(using LocalUid): Delta = {
    if parent != ReplicatedTree.rootDot && !compact.contains(parent) then {
      throw new IllegalArgumentException(s"Dot $parent does not exist in the tree")
    }

    val dot =
      observed.nextDot(LocalUid.replicaId)

    ReplicatedTree(
      elements =
        Map(dot -> ReplicatedTree.Node(
          dot,
          parent,
          Map(parent -> 0),
          value(dot)
        )),
    )
  }

  def update(dot: Dot, newValue: A): Delta = {
    node(dot) match {
      case Some(n) =>
        ReplicatedTree(
          elements = Map(
            dot -> n.copy(value = newValue)
          )
        )
      case None =>
        ReplicatedTree.empty
    }
  }

  def delete(dot: Dot): Delta = {
    def collectChildrenDots(toDelete: Dot, acc: Dots): Dots = {
      children(toDelete)
        .map(_.dot)
        .foldLeft(acc `union` Dots.single(toDelete)) { (currentAcc, childDot) =>
          collectChildrenDots(childDot, currentAcc)
        }
    }

    // TODO: do we also need to cleanup the edges maps?
    ReplicatedTree(
      elements = Map.empty,
      removed = collectChildrenDots(dot, Dots.single(dot))
    )
  }

  def clear(): Delta = {
    ReplicatedTree(
      elements = Map.empty,
      removed = observed
    )
  }

  def move(dot: Dot, newParent: Dot): Delta = {
    node(dot).fold(ReplicatedTree.empty) { n =>
      val edits       = ensureNodeIsRooted(n.parent) ::: ensureNodeIsRooted(newParent) ::: List((dot, newParent))
      val newElements = edits.map { case (dot, parent) =>
        val node  = elements(dot)
        val edges = node.edges + (parent -> (node.maxCounter + 1))
        dot -> node.copy(parent = parent, edges = edges)
      }.toMap
      ReplicatedTree(elements = newElements)
    }
  }

  private def isBelowNode(node: Dot, other: Dot): Boolean = {
    if node == other then return true
    var tortoise = node
    var hare     = parent(node)
    while hare.isDefined && hare.get != other do {
      if tortoise == hare.get then return false
      hare = parent(hare.get)
      if hare.isEmpty || hare.get == other then return hare.contains(other)
      tortoise = parent(tortoise).get
      hare = parent(hare.get)
    }
    hare.contains(other)
  }

  private def ensureNodeIsRooted(dot: Dot): List[(Dot, Dot)] = {
    node(dot).fold(List.empty[(Dot, Dot)]) { n =>
      if n.parent == ReplicatedTree.rootDot then List.empty
      else {
        val edge    = n.largestEdge
        val current = if edge != n.parent then List((n.dot, n.parent)) else List.empty
        current ::: ensureNodeIsRooted(n.parent)
      }
    }
  }
}

object ReplicatedTree {
  val rootDot = Dot.zero

  case class Node[A](dot: Dot, parent: Dot, edges: Map[Dot, Int], value: A) {
    def maxCounter: Int =
      edges.values.maxOption.getOrElse(-1)

    def largestEdge: Dot =
      edges.maxByOption(_._2).map(_._1).getOrElse(parent)
  }

  given nodeLattice[A]: Lattice[Node[A]] = {
    given Lattice[A] = Lattice.assertEquals
    given Lattice[Dot] with {
      def merge(left: Dot, right: Dot): Dot = {
        // we can always choose left, since we re-parent based on largest edge after merging
        left
      }
    }
    given Lattice[Map[Dot, Int]] = {
      given Lattice[Int] = math.max
      Lattice.mapLattice
    }
    Lattice.derived
  }

  def empty[A]: ReplicatedTree[A] = ReplicatedTree[A](Map.empty, Dots.empty)

  given lattice[A]: Lattice[ReplicatedTree[A]] with {
    given mapLattice: Lattice[Map[Dot, ReplicatedTree.Node[A]]]                     = Lattice.mapLattice
    def merge(left: ReplicatedTree[A], right: ReplicatedTree[A]): ReplicatedTree[A] = {
      val elements = left.elements `merge` right.elements
      val deleted  = left.removed `union` right.removed
      resolveConflicts(ReplicatedTree(
        elements,
        deleted
      ))
    }
  }

  private def resolveConflicts[A](mergedTree: ReplicatedTree[A]): ReplicatedTree[A] = {
    var tree = mergedTree.copy(elements = mergedTree.compact.map({
      case (dot, node) =>
        (dot, node.copy(parent = node.largestEdge))
    }))

    val nonRootedNodes = findNonRootedNodes(tree)
    if nonRootedNodes.isEmpty then return tree

    val parentUpdates = computeParentUpdates(tree, nonRootedNodes)
    tree.copy(elements = tree.elements.map {
      case (dot, node) =>
        parentUpdates.get(dot) match {
          case Some(newParent) => (dot, node.copy(parent = newParent))
          case None            => (dot, node)
        }
    })
  }

  private def findNonRootedNodes[A](tree: ReplicatedTree[A]): mutable.Set[Dot] = {
    val nonRootedNodes = mutable.Set[Dot]()
    tree.elements.values
      .filterNot(node => tree.isBelowNode(node.dot, ReplicatedTree.rootDot))
      .foreach { node =>
        var current: Option[Dot] = Some(node.dot)
        while current.isDefined && nonRootedNodes.add(current.get) do
          current = tree.parent(current.get)
      }
    nonRootedNodes
  }

  private def computeParentUpdates[A](
      tree: ReplicatedTree[A],
      nonRootedNodes: mutable.Set[Dot]
  ): mutable.Map[Dot, Dot] = {
    case class PQItem(child: Dot, parent: Dot, counter: Int) extends Ordered[PQItem] {
      def compare(that: PQItem): Int = {
        given dotOrdering: Ordering[Dot] = Ordering.by((d: Dot) => (d.time, d.place.delegate))
        Ordering
          .by((i: PQItem) => (i.counter, i.parent, i.child))
          .compare(this, that)
      }
    }

    val deferredEdges = mutable.Map[Dot, mutable.ListBuffer[PQItem]]()
    val readyEdges    = mutable.PriorityQueue[PQItem]()
    nonRootedNodes.foreach { child =>
      tree.node(child) match {
        case Some(node) =>
          node.edges.foreach { (parent, edgeCounter) =>
            val item = PQItem(child, parent, edgeCounter)
            if !nonRootedNodes.contains(parent) then readyEdges.enqueue(item)
            else deferredEdges.getOrElseUpdate(parent, mutable.ListBuffer()) += item
          }
        case None =>
      }
    }

    val parentUpdates = mutable.Map[Dot, Dot]()
    while readyEdges.nonEmpty do {
      val top = readyEdges.dequeue()
      if nonRootedNodes.remove(top.child) then {
        parentUpdates(top.child) = top.parent
        deferredEdges.remove(top.child) match {
          case Some(edges) => edges.foreach(readyEdges.enqueue(_))
          case None        =>
        }
      }
    }
    parentUpdates
  }
}
