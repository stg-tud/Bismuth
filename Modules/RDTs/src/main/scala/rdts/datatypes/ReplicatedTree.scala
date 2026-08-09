package rdts.datatypes

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.ReplicatedTree.Node
import rdts.time.{Dot, Dots}

import scala.collection.mutable as mutable

case class ReplicatedTree[A](
    elements: Map[Dot, ReplicatedTree.Node[A]],
    removed: Dots = Dots.empty
) {
  type Delta = ReplicatedTree[A]

  lazy val observed: Dots = elements.keySet.foldLeft(removed)((acc, dot) => acc `union` Dots.single(dot))

  lazy val compact: Map[Dot, Node[A]] = elements.filter((k, _) => !removed.contains(k))

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

  def children(dot: Dot): Iterable[ReplicatedTree.Node[A]] =
    compact.values.filter(n => n.parent == dot)

  def insert(parent: Dot, value: A)(using LocalUid): Delta =
    insertWith(parent, _ => value)

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

  def update(dot: Dot, newValue: A): Delta =
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

  def delete(dot: Dot): Delta = {
    def collectChildrenDots(toDelete: Dot, acc: Dots): Dots =
      children(toDelete)
        .map(_.dot)
        .foldLeft(acc `union` Dots.single(toDelete)) { (currentAcc, childDot) =>
          collectChildrenDots(childDot, currentAcc)
        }

    // TODO: do we also need to cleanup the edges maps?
    ReplicatedTree(
      elements = Map.empty,
      removed = collectChildrenDots(dot, Dots.single(dot))
    )
  }

  def clear(): Delta =
    ReplicatedTree(
      elements = Map.empty,
      removed = observed
    )

  def move(dot: Dot, newParent: Dot): Delta =
    node(dot).fold(ReplicatedTree.empty) { n =>
      val edits       = ensureNodeIsRooted(n.parent) ::: ensureNodeIsRooted(newParent) ::: List((dot, newParent))
      val newElements = edits.map { case (dot, parent) =>
        val node  = elements(dot)
        val edges = node.edges + (parent -> (node.maxCounter + 1))
        dot -> node.copy(parent = parent, edges = edges)
      }.toMap
      ReplicatedTree(elements = newElements)
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

  private def ensureNodeIsRooted(dot: Dot): List[(Dot, Dot)] =
    node(dot).fold(List.empty[(Dot, Dot)]) { n =>
      if n.parent == ReplicatedTree.rootDot then List.empty
      else {
        val edge    = n.largestEdge
        val current = if edge != n.parent then List((n.dot, n.parent)) else List.empty
        current ::: ensureNodeIsRooted(n.parent)
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

  given nodeLattice[A: Lattice]: Lattice[Node[A]] = {
    given Lattice[Dot] with {
      def merge(left: Dot, right: Dot): Dot =
        // we can always choose left, since we re-parent based on largest edge after merging
        left
    }
    given Lattice[Map[Dot, Int]] = {
      given Lattice[Int] = math.max
      Lattice.mapLattice
    }
    Lattice.derived
  }

  def empty[A]: ReplicatedTree[A] = ReplicatedTree[A](Map.empty, Dots.empty)

  // Timing stats for diagnosing ReplicatedTree merge performance
  object MergeTimings {
    var mergeElementsNanos: Long        = 0
    var unionRemovedNanos: Long         = 0
    var resolveConflictsNanos: Long     = 0
    var compactNanos: Long              = 0
    var findNonRootedNanos: Long        = 0
    var computeParentUpdatesNanos: Long = 0
    var applyParentUpdatesNanos: Long   = 0
    var callCount: Long                 = 0

    def reset(): Unit = {
      mergeElementsNanos = 0
      unionRemovedNanos = 0
      resolveConflictsNanos = 0
      compactNanos = 0
      findNonRootedNanos = 0
      computeParentUpdatesNanos = 0
      applyParentUpdatesNanos = 0
      callCount = 0
    }

    def report(): String = {
      if callCount == 0 then return "No ReplicatedTree merges"
      f"""ReplicatedTree.merge breakdown (${callCount} calls):
         |  mergeElements:      ${mergeElementsNanos / 1_000_000.0 / callCount}%.4fms avg
         |  unionRemoved:       ${unionRemovedNanos / 1_000_000.0 / callCount}%.4fms avg
         |  resolveConflicts:   ${resolveConflictsNanos / 1_000_000.0 / callCount}%.4fms avg
         |    compact:          ${compactNanos / 1_000_000.0 / callCount}%.4fms avg
         |    findNonRooted:    ${findNonRootedNanos / 1_000_000.0 / callCount}%.4fms avg
         |    computeUpdates:   ${computeParentUpdatesNanos / 1_000_000.0 / callCount}%.4fms avg
         |    applyUpdates:     ${applyParentUpdatesNanos / 1_000_000.0 / callCount}%.4fms avg""".stripMargin
    }
  }

  given lattice[A: Lattice]: Lattice[ReplicatedTree[A]] with {
    given mapLattice: Lattice[Map[Dot, ReplicatedTree.Node[A]]]                     = Lattice.mapLattice
    def merge(left: ReplicatedTree[A], right: ReplicatedTree[A]): ReplicatedTree[A] = {
      MergeTimings.callCount += 1

      var t0       = System.nanoTime()
      val elements = left.elements `merge` right.elements
      MergeTimings.mergeElementsNanos += System.nanoTime() - t0

      t0 = System.nanoTime()
      val deleted = left.removed `union` right.removed
      MergeTimings.unionRemovedNanos += System.nanoTime() - t0

      t0 = System.nanoTime()
      if right.size == 1 then {
        val nodeOnLeft = left.node(right.nodes.head.dot)
        if nodeOnLeft.isDefined && nodeOnLeft.get.largestEdge == right.nodes.head.largestEdge then {
          return ReplicatedTree(elements, deleted)
        }
        // New node added
        if nodeOnLeft.isEmpty then {
          return ReplicatedTree(elements, deleted)
        }
      }
      if right.size == 0 && right.removed.size == 1 then {
        // Node removed
        return ReplicatedTree(elements, deleted)
      }

      val result = resolveConflicts(ReplicatedTree(
        elements,
        deleted
      ))
      MergeTimings.resolveConflictsNanos += System.nanoTime() - t0
      result
    }
  }

  private def resolveConflicts[A](mergedTree: ReplicatedTree[A]): ReplicatedTree[A] = {
    var t0   = System.nanoTime()
    var tree = mergedTree.copy(elements = mergedTree.compact.map {
      case (dot, node) =>
        (dot, node.copy(parent = node.largestEdge))
    })
    MergeTimings.compactNanos += System.nanoTime() - t0

    t0 = System.nanoTime()
    val nonRootedNodes = findNonRootedNodes(tree)
    MergeTimings.findNonRootedNanos += System.nanoTime() - t0

    if nonRootedNodes.isEmpty then return tree

    t0 = System.nanoTime()
    val parentUpdates = computeParentUpdates(tree, nonRootedNodes)
    MergeTimings.computeParentUpdatesNanos += System.nanoTime() - t0

    t0 = System.nanoTime()
    val result = tree.copy(elements = tree.elements.map {
      case (dot, node) =>
        (dot, parentUpdates.get(dot).map(p => node.copy(parent = p)).getOrElse(node))
    })
    MergeTimings.applyParentUpdatesNanos += System.nanoTime() - t0

    result
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

  private case class PQItem(child: Dot, parent: Dot, counter: Int)

  private given dotOrdering: Ordering[Dot]       = Ordering.by((d: Dot) => (d.time, d.place.delegate))
  private given pqItemOrdering: Ordering[PQItem] = Ordering.by((i: PQItem) => (i.counter, i.parent, i.child))

  private def computeParentUpdates[A](
      tree: ReplicatedTree[A],
      nonRootedNodes: mutable.Set[Dot]
  ): mutable.Map[Dot, Dot] = {
    val deferredEdges = mutable.Map[Dot, mutable.ArrayBuffer[PQItem]]()
    val readyEdges    = mutable.PriorityQueue[PQItem]()
    nonRootedNodes.foreach { child =>
      tree.node(child) match {
        case Some(node) =>
          node.edges.foreach { (parent, edgeCounter) =>
            val item = PQItem(child, parent, edgeCounter)
            if !nonRootedNodes.contains(parent) then readyEdges.enqueue(item)
            else deferredEdges.getOrElseUpdate(parent, mutable.ArrayBuffer()) += item
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
