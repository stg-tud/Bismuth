package rdts.datatypes

import rdts.time.Dot
import rdts.base.LocalUid
import rdts.time.Dots
import rdts.base.Lattice

case class ReplicatedTree[A](
    elements: Map[Dot, ReplicatedTree.Node[A]],
    deleted: Dots = Dots.empty
) {
  type Delta = ReplicatedTree[A]

  lazy val observed = elements.keySet.foldLeft(deleted)((acc, dot) => acc `union` Dots.single(dot))

  lazy val compact = elements.filter((k, _) => !deleted.contains(k))

  def isEmpty: Boolean = compact.isEmpty

  def size: Int = compact.size

  def nodes: Iterable[ReplicatedTree.Node[A]] = compact.values

  def node(dot: Dot): Option[ReplicatedTree.Node[A]] = elements.get(dot)

  def parent(dot: Dot): Option[Dot] = elements.get(dot).map(_.parent)

  def root: Option[ReplicatedTree.Node[A]] = {
    val c = children(ReplicatedTree.rootDot)
    assert(c.size <= 1)
    c.headOption
  }

  def children(dot: Dot): Iterable[ReplicatedTree.Node[A]] = {
    elements.values.filter(_.parent == dot)
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
        Map(dot -> ReplicatedTree.Node(dot, parent, Map(parent -> ReplicatedTree.EdgeCounter(dot, 0)), value(dot))),
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
    // TODO: do we also need to cleanup the edges maps?
    ReplicatedTree(
      elements = Map.empty,
      deleted = Dots.single(dot)
    )
  }

  def clear(): Delta = {
    ReplicatedTree(
      elements = Map.empty,
      deleted = observed
    )
  }

  def move(dot: Dot, newParent: Dot): Delta = {
    node(dot) match {
      case Some(child) =>
        val oldParent = child.parent
        val edits     = ensureNodeIsRooted(oldParent) :::
          ensureNodeIsRooted(newParent) ::: List((dot, newParent))

        val newElements = edits.map { case (childDot, parent) =>
          val childNode  = node(childDot).get
          val maxCounter = childNode.maxCounter
          (
            childDot,
            childNode.copy(
              parent = parent,
              edges = childNode.edges + (parent -> ReplicatedTree.EdgeCounter(parent, maxCounter + 1))
            )
          )
        }.toMap

        return ReplicatedTree(
          elements = newElements
        )
      case None =>
        return ReplicatedTree.empty
    }
  }

  private def isBelowNode(node: Dot, other: Dot): Boolean = {
    if node == other then {
      return true
    }
    var tortoise = node
    var hare     = parent(node)
    while hare.isDefined && hare.get != other do {
      if tortoise == hare.get then {
        return false
      }
      hare = parent(hare.get)
      if hare.isEmpty || hare.get == other then {
        return hare.contains(other)
      }
      tortoise = parent(tortoise).get
      hare = parent(hare.get)
    }
    hare.contains(other)
  }

  private def ensureNodeIsRooted(dot: Dot): List[(Dot, Dot)] = {
    node(dot) match {
      case Some(child) => {
        if child.parent == ReplicatedTree.rootDot then {
          List.empty
        } else {
          val edge = child.largestEdge
          if edge != child.parent then {
            (child.dot, edge) :: ensureNodeIsRooted(edge)
          } else {
            ensureNodeIsRooted(edge)
          }
        }
      }
      case None => List.empty
    }
  }
}

object ReplicatedTree {
  val rootDot = Dot.zero

  case class EdgeCounter(dot: Dot, counter: Int)

  case class Node[A](dot: Dot, parent: Dot, edges: Map[Dot, EdgeCounter], value: A) {
    def maxCounter: Int = {
      if edges.isEmpty then -1 else edges.maxBy(_._2.counter)._2.counter
    }

    def largestEdge: Dot = {
      if edges.isEmpty then {
        parent
      } else {
        edges.maxBy(_._2.counter)._1
      }
    }
  }

  def empty[A]: ReplicatedTree[A] = ReplicatedTree[A](Map.empty, Dots.empty)

  given lattice[A]: Lattice[ReplicatedTree[A]] with {
    def merge(left: ReplicatedTree[A], right: ReplicatedTree[A]): ReplicatedTree[A] = {
      val elements = left.elements ++ right.elements
      val deleted  = left.deleted `union` right.deleted
      recomputeParentChildren(ReplicatedTree(
        elements,
        deleted
      ))
    }
  }

  private def recomputeParentChildren[A](state: ReplicatedTree[A]): ReplicatedTree[A] = {
    case class PQItem(child: Dot, parent: Dot, counter: Int) extends Ordered[PQItem] {
      def compare(that: PQItem): Int = {
        given dotOrdering: Ordering[Dot] {
          def compare(x: Dot, y: Dot): Int = {
            val counterComparison = x.time.compare(y.time)
            if counterComparison != 0 then counterComparison
            else x.place.delegate.compare(y.place.delegate)
          }
        }

        val counterCmp = this.counter.compare(that.counter)
        if counterCmp != 0 then counterCmp
        else {
          val parentCmp = dotOrdering.compare(this.parent, that.parent)
          if parentCmp != 0 then parentCmp
          else dotOrdering.compare(this.child, that.child)
        }
      }
    }

    var newState = state.copy(elements = state.compact.map({
      case (dot, node) =>
        (dot, node.copy(parent = node.largestEdge))
    }))

    val nonRootedNodes = scala.collection.mutable.Set[Dot]()
    for node <- newState.elements.values do {
      if !nonRootedNodes.contains(node.parent) && !newState.isBelowNode(node.dot, ReplicatedTree.rootDot) then {
        var nodeId: Option[Dot] = Some(node.dot)
        while nodeId.isDefined do {
          val currentNode = nodeId.get
          if !nonRootedNodes.contains(currentNode) then {
            nonRootedNodes.add(currentNode)
            nodeId = newState.parent(currentNode)
          } else {
            nodeId = None
          }
        }
      }
    }

    if nonRootedNodes.isEmpty then {
      return newState
    }

    val deferredEdges = scala.collection.mutable.Map[Dot, scala.collection.mutable.ListBuffer[PQItem]]()
    val readyEdges    = scala.collection.mutable.PriorityQueue[PQItem]()

    for child <- nonRootedNodes do {
      newState.node(child) match {
        case Some(node) =>
          for (parent, edgeCounter) <- node.edges do {
            val item = PQItem(child, parent, edgeCounter.counter)
            if !nonRootedNodes.contains(parent) then {
              readyEdges.enqueue(item)
            } else {
              deferredEdges.getOrElseUpdate(parent, scala.collection.mutable.ListBuffer[PQItem]()) += item
            }
          }
        case None =>
      }
    }

    while readyEdges.nonEmpty do {
      val top   = readyEdges.dequeue()
      val child = top.child
      if !nonRootedNodes.contains(child) then {}
      else {
        newState = newState.copy(elements = newState.elements.updatedWith(child) {
          case Some(node) =>
            nonRootedNodes.remove(child)
            deferredEdges.remove(child) match {
              case Some(deferred) =>
                for edge <- deferred do {
                  readyEdges.enqueue(edge)
                }
              case None =>
            }
            Some(node.copy(parent = top.parent))
          case None =>
            None
        })
      }
    }

    newState
  }
}
