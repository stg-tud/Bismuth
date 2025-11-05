package replication

import rdts.base.Uid
import rdts.time.{Dot, Dots}
import replication.DotTreeNode.RootNode
import replication.ProtocolMessage.Payload

import scala.annotation.tailrec

sealed trait DotTreeNode

object DotTreeNode {

  case class RootNode(var dots: Dots, var successors: Set[DotTreeNode.Node] = Set.empty) extends DotTreeNode {
    def addDot(dot: Dot): Unit =
      RootNode.this.dots = RootNode.this.dots.add(dot)
  }

  /** @param dot              The timestamp of the dot
    * @param predecessorNode   The predecessor in the tree structure
    * @param causalPredecessor The dot emitted by the peer before the dot
    * @param successorNode     The successor in the tree structure
    * @param knownBy           Set of peers that are aware of the dot
    */
  case class Node(
      dot: Dot,
      var predecessorNode: DotTreeNode,
      causalPredecessor: Dot,
      var successorNode: Option[DotTreeNode.Node],
      var knownBy: Set[Uid] = Set.empty
  ) extends DotTreeNode {
    override def equals(other: Any): Boolean = other match {
      case that: Node => this.dot == that.dot
      case _          => false
    }

    override def hashCode(): Int = dot.hashCode()

    override def toString: String = {
      val dotString             = dot
      val predecessorNodeString = predecessorNode match {
        case node: Node if node == this => "this"
        case node: Node                 => s"Node(${node.dot})"
        case _: RootNode                => "RootNode"
      }
      val causalPredecessorString = causalPredecessor
      val successorNodeString     = successorNode match {
        case Some(node) if node == this => "this"
        case Some(node)                 => s"Node(${node.dot})"
        case None                       => "None"
      }
      s"($dotString, $predecessorNodeString, $causalPredecessorString, $successorNodeString, $knownBy)"
    }
  }

  extension (nodes: IterableOnce[DotTreeNode.Node])
      def toDots: Dots = nodes.iterator.foldLeft(Dots.empty)((acc, node) => acc.add(node.dot))

}

class DotTree {
  val rootNode: RootNode                         = RootNode(Dots.empty)
  var causalBarriers: Map[Uid, DotTreeNode.Node] = Map.empty
  var leaves: Map[Uid, DotTreeNode.Node]         = Map.empty

  def getLeafDot(uid: Uid): Option[Dot] = leaves.get(uid) match {
    case Some(treeNode) => Option(treeNode.dot)
    case None           => None
  }

  def getLeaf(uid: Uid): Option[DotTreeNode.Node] = leaves.get(uid)

  /** Searches for the node in the tree that contains the dot
    *
    * @param dot         The dot for whom to find the node
    * @param currentNode The current node where to check if it contains the dot
    * @return A tuple containing the node that holds the dot and the successor of that node
    */
  @tailrec
  private def getNodeOfDot(dot: Dot, currentNode: DotTreeNode): Option[DotTreeNode] = currentNode match {
    case RootNode(dots, _) => if dots.contains(dot) then Option(currentNode) else None
    case node @ DotTreeNode.Node(startNodeDot, predecessor, _, _, _) =>
      if startNodeDot == dot then Option(currentNode) else getNodeOfDot(dot, predecessor)
  }

  def getNodeOfDot(dot: Dot): Option[DotTreeNode] = getLeaf(dot.place) match {
    case Some(leaf) => getNodeOfDot(dot, leaf)
    case None       => None
  }

  def addNode(dot: Dot, causalPredecessor: Dot): Boolean = {
    if getNodeOfDot(dot).isDefined then return false

    val leafNodeOption = getLeaf(dot.place)
    if leafNodeOption.isEmpty && dot == causalPredecessor && dot.time == 0 then
        return {
          // initial message
          val node = DotTreeNode.Node(dot, rootNode, causalPredecessor, None)
          rootNode.successors += node
          leaves += (dot.place         -> node)
          causalBarriers += (dot.place -> node)
          true
        }
    else if leafNodeOption.isDefined && dot == causalPredecessor && dot.time == 0 then
        return {
          val leafNode = leafNodeOption.get
          if leafNode.causalPredecessor == dot then {
            val node = DotTreeNode.Node(dot, rootNode, causalPredecessor, Some(leafNode))
            rootNode.successors += node
            leafNode.predecessorNode = node
            causalBarriers += (dot.place -> leafNode)
            true
          } else false
        }
    else if dot == causalPredecessor || dot.time == 0 then return false

    val causalPredecessorNodeOption = getNodeOfDot(causalPredecessor)
    causalPredecessorNodeOption match {
      case Some(causalPredecessorNode @ DotTreeNode.Node(_, _, _, Some(successorNode), _))
          if successorNode.dot == dot => false // duplicate
      case Some(causalPredecessorNode @ DotTreeNode.Node(_, _, _, Some(successorNode), _))
          if successorNode.dot != dot =>
        // dot is the missing link between causalPredecessorNode and its successorNode
        val node = DotTreeNode.Node(dot, causalPredecessorNode, causalPredecessor, Some(successorNode))
        causalPredecessorNode.successorNode = Some(node)
        successorNode.predecessorNode = node
        // TODO: causal barrier might need to be shifted
        shiftCausalBarrier(dot.place)
        true
      case Some(causalPredecessorNode @ DotTreeNode.Node(_, _, _, None, _)) =>
        // causal predecessor node must be a leaf, otherwise it would have a successor
        val node = DotTreeNode.Node(dot, causalPredecessorNode, causalPredecessor, None)
        causalPredecessorNode.successorNode = Some(node)
        leaves += (dot.place -> node)
        shiftCausalBarrier(dot.place)
        true
      case Some(causalPredecessorNode @ RootNode(dots, _)) if dots.contains(dot)  => false // duplicate
      case Some(causalPredecessorNode @ RootNode(dots, _)) if !dots.contains(dot) =>
        leafNodeOption match {
          case Some(leafNode) =>
            // dot must be a predecessor of the leaf
            val node = DotTreeNode.Node(dot, causalPredecessorNode, causalPredecessor, Some(leafNode))
            leafNode.predecessorNode = node
          case None =>
            // dot is the new leaf
            val node = DotTreeNode.Node(dot, causalPredecessorNode, causalPredecessor, None)
            leaves += (dot.place -> node)
        }
        shiftCausalBarrier(dot.place)
        true
      case None if leafNodeOption.exists(leaf => leaf.dot.time < dot.time) =>
        // dot is the new leaf
        val leaf = leafNodeOption.get
        val node = DotTreeNode.Node(dot, leaf, causalPredecessor, None)
        leaf.successorNode = Option(node)
        leaves += (dot.place -> node)
        true
      case None if leafNodeOption.exists(leaf => leaf.dot.time > dot.time) =>
        var startNode: Option[DotTreeNode] = leafNodeOption
        while startNode.isDefined do {
          startNode match {
            case Some(node @ DotTreeNode.Node(currentDot, _, _, Some(successorNode), _))
                if currentDot.time < dot.time =>
              val newNode = DotTreeNode.Node(dot, node, causalPredecessor, Option(successorNode))
              node.successorNode = Some(newNode)
              successorNode.predecessorNode = newNode
              startNode = None
            case Some(DotTreeNode.Node(currentDot, causalPredecessorNode, _, _, _)) if currentDot.time > dot.time =>
              startNode = Option(causalPredecessorNode)
            case _ => return false
          }
        }
        true
      case None =>
        // dot is the new leaf
        val node = DotTreeNode.Node(dot, rootNode, causalPredecessor, None)
        rootNode.successors += node
        leaves += (dot.place -> node)
        true
      case _ => false
    }
  }

  def shiftCausalBarrier(branch: Uid): Unit = {
    var currentNode = causalBarriers(branch)
    while currentNode.successorNode.exists(successor => successor.causalPredecessor == currentNode.dot) do
        currentNode = currentNode.successorNode.get
    causalBarriers += (branch -> currentNode)
  }

  /** add new leaf to branch of this peer
    * @param newLeaf the new dot of the new leaf
    * @param uid     the uid of this peer
    */
  def addNewLeaf(newLeaf: Dot, uid: Uid): Unit = {
    val (predecessorDot, predecessorNode) = getLeaf(uid) match {
      case Some(leaf) => (leaf.dot, leaf)
      case None       => (rootNode.dots.clockOf(uid).getOrElse(Dot(uid, 0)), rootNode)
    }
    val node         = DotTreeNode.Node(newLeaf, predecessorNode, predecessorDot, None)
    leaves += (uid         -> node)
    causalBarriers += (uid -> node)
  }

  @tailrec
  private def collectBranch(dots: Dots, node: DotTreeNode): Dots = node match {
    case RootNode(_, _)                              => dots
    case DotTreeNode.Node(dot, predecessor, _, _, _) => collectBranch(dots.add(dot), predecessor)
  }

  def collapse: Dots = leaves.foldLeft(rootNode.dots)((dots, leaf) => collectBranch(dots, leaf._2))

  def getLeavesAsDots: Dots = leaves.foldLeft(Dots.empty)((dots, leaf) => dots.add(leaf._2.dot))

  def getCausalBarriersAsDots: Dots =
    causalBarriers.foldLeft(Dots.empty)((dots, causalBarrier) => dots.add(causalBarrier._2.dot))

  override def toString: String = {
    var string = "leaves:\n"
    leaves.foreach(leaf => string += s"\t${leaf._1} -> ${leaf._2}\n")
    string += s"root: $rootNode"
    string
  }

  def toTreeLikeString: String = {
    var string = s"root: ${rootNode.dots}\n"

    def inner(node: DotTreeNode.Node): String = node.successorNode match {
      case Some(successorNode) => s"-> ${node.dot} ${inner(successorNode)}"
      case None                => s"-> ${node.dot} -> None\n"
    }

    rootNode.successors.foreach(node => string += inner(node))
    string
  }

  @tailrec
  final def getPredecessors(node: DotTreeNode, predecessors: List[Dot]): List[Dot] = node match {
    case RootNode(dots, _)                           => predecessors
    case DotTreeNode.Node(dot, predecessor, _, _, _) => getPredecessors(predecessor, predecessors :+ dot)
  }

  def getNextDot(uid: Uid): Dot = getLeaf(uid) match {
    case Some(node) => node.dot.advance
    case None       => Dot(uid, 0)
  }

  @tailrec
  private def getCompleteBranchWithoutRoot(currentNode: DotTreeNode, slice: Dots): Dots = currentNode match {
    case RootNode(dots, _)                           => slice
    case DotTreeNode.Node(dot, predecessor, _, _, _) => getCompleteBranchWithoutRoot(predecessor, slice.add(dot))
  }

  @tailrec
  private def getSliceOfBranch(startNode: DotTreeNode, stop: Dot, slice: Dots): Dots = startNode match {
    case RootNode(dots, _) =>
      if dots.contains(stop) then slice.merge(dots)
      else throw IllegalStateException(s"Dot $stop not yet known to this replica")
    case DotTreeNode.Node(dot, predecessor, _, _, _) if dot == stop => slice
    case DotTreeNode.Node(dot, predecessor, _, _, _) => getSliceOfBranch(predecessor, stop, slice.add(dot))
  }

  def getUnknownDotsForPeer(peer: Uid, known: Dots): Dots = {
    val knownPeers       = leaves.keySet
    val knownPeersOfPeer = known.peers

    // the other peer is aware of more peers than this
//    val diffSet = knownPeersOfPeer.diff(knownPeers)
//    if diffSet.nonEmpty then
//      throw IllegalStateException(s"Peer is aware of more peers than this replica: $diffSet")

    var leavesOfPeer: Map[Uid, DotTreeNode.Node] = Map.empty
    var unknownPeersByPeer: Set[Uid]             = Set.empty
    var unknownDotsByPeer: Dots                  = Dots.empty

    leaves.foreach { (uid, leaf) =>
      known.clockOf(uid) match {
        case Some(leafKnownPeer) if leaf.dot.time < leafKnownPeer.time =>
          throw IllegalStateException(
            s"Knowledge of peer augments local knowledge on branch of peer: $uid. Last known dot locally: $leaf vs dot known by peer: $leafKnownPeer"
          ) // the dot known by the peer is more advanced than the dot known by this
        case Some(leafKnownPeer) if leaf.dot.time > leafKnownPeer.time =>
          unknownDotsByPeer = getSliceOfBranch(leaf, leafKnownPeer, unknownDotsByPeer)
        case Some(leafKnownPeer) => // knowledge of peer is up to date with this
        case None                => unknownPeersByPeer += uid
      }
    }

    var addRoot = false
    unknownPeersByPeer.foreach { peer =>
      addRoot = rootNode.dots.max(peer).isDefined
      unknownDotsByPeer = getCompleteBranchWithoutRoot(getLeaf(peer).get, unknownDotsByPeer)
    }
    if addRoot then unknownDotsByPeer = unknownDotsByPeer.merge(rootNode.dots)

    unknownDotsByPeer
  }

  def getCausalBarriers: Dots = causalBarriers.values.foldLeft(Dots.empty)((dots, node) => dots.add(node.dot))

  def updateKnowledgeOfPeer(peerUid: Uid, lastKnownDot: Dot): Boolean = {
    getNodeOfDot(lastKnownDot) match {
      case Some(node @ DotTreeNode.Node(_, _, _, _, _)) =>
        node.knownBy += peerUid
        var predecessor = node.predecessorNode
        while predecessor != rootNode do {
          predecessor.asInstanceOf[replication.DotTreeNode.Node].knownBy += peerUid
          predecessor = predecessor.asInstanceOf[replication.DotTreeNode.Node].predecessorNode
        }
        true
      case _ => false
    }
  }

  /** Collapse all dots that are known by all peers, as well as their predecessors, into the root node */
  def collapseGeneralKnowledge(): Unit = {
    val numberOfKnownPeers = leaves.keySet.size - 1 // all nodes in the tree are known by itself
    rootNode.successors.foreach { startNode =>
      var currentNode = Option(startNode)
      while currentNode.exists(node =>
            node.knownBy.size == numberOfKnownPeers && node.successorNode.exists(successorNode =>
              successorNode.causalPredecessor == node.dot
            )
          )
      do {
        val nextNode = currentNode.get.successorNode
        rootNode.successors -= currentNode.get
        rootNode.addDot(currentNode.get.dot)
        rootNode.successors += nextNode.get
        nextNode.get.predecessorNode = rootNode
        currentNode = nextNode
      }
    }
  }
}

class DeltaTreeContext[State](selfUid: Uid) {

  val tree: DotTree = DotTree()

  private var payloads: List[CachedMessage[Payload[State]]] = List.empty
  // look up table to index of cached messages
  private var dotToPayload: Map[Dot, List[Int]] = Map.empty
  // the dots (emitted by this replica) known by the peers, can be later used to remove dots
//  private var knowledgeOfPeers: Map[Uid, Dot] = Map.empty

  def addNode(dot: Dot, causalPredecessor: Dot): Boolean = tree.addNode(dot, causalPredecessor)

  def getLeaf(uid: Uid): Option[Dot] = tree.getLeafDot(uid)

  def getSelfLeaf: Option[Dot] = tree.getLeafDot(selfUid)

  /** adds all dots to the tree that are not yet present
    *
    * @param dots                      The set of dots to add
    * @param initialCausalPredecessors Dots where the le causal predecessor of the initial dot
    * @return
    */
  def addNonRedundant(dots: Dots, initialCausalPredecessors: Dots): Dots = {
    var nonRedundantDots  = Dots.empty
    var currentPeer       = selfUid
    var causalPredecessor = Dot.zero
    dots.iterator.filter(dot => dot.place != selfUid).foreach { dot =>
      if dot.place != currentPeer && dot.time > 0 then {
        currentPeer = dot.place
        causalPredecessor = initialCausalPredecessors.max(currentPeer) match {
          case Some(value) => value
          case None        => throw IllegalStateException(
              s"$selfUid: No causal predecessor given for dot $dot, initial causal predecessors: $initialCausalPredecessors"
            )
        }
      } else if dot.place != currentPeer then {
        // special case of the first dot that does not have a predecessor
        currentPeer = dot.place
        causalPredecessor = dot
      }
      if tree.addNode(dot, causalPredecessor) then {
        nonRedundantDots = nonRedundantDots.add(dot)
        causalPredecessor = dot
      }
    }
    nonRedundantDots
  }

  private def addDotToPayload(index: Int, dot: Dot): Unit = {
    val newIndices = dotToPayload.get(dot) match {
      case Some(indices) => indices :+ index
      case None          => List.apply(index)
    }
    dotToPayload += (dot -> newIndices)
  }

  def storeMessage(dots: Dots, message: CachedMessage[Payload[State]]): Unit = {
    payloads = payloads :+ message
    val index = payloads.size - 1
    dots.internal.foreach { (uid, range) =>
      range.inner.foreach { time =>
        addDotToPayload(index, Dot(uid, time))
      }
    }
  }

  def storeOutgoingMessage(dot: Dot, message: CachedMessage[Payload[State]]): Unit = {
    // store dot in tree structure
    tree.addNewLeaf(dot, selfUid)

    // store message
    payloads = payloads :+ message
    addDotToPayload(payloads.size - 1, dot)
  }

  def getUnknownDotsForPeer(peer: Uid, known: Dots): Dots = tree.getUnknownDotsForPeer(peer, known)

  def getPayloads(dots: Dots): List[CachedMessage[Payload[State]]] = {
    val indices = dotToPayload.filter((dot, _) => dots.contains(dot)).flatMap((_, msgs) => msgs).toSet
    payloads.zipWithIndex.collect { case (msgs, i) if indices.contains(i) => msgs }
  }

  def getAllPayloads: List[CachedMessage[Payload[State]]] = payloads

  def getSelfContext: Dots = tree.collapse

  def getNextDot: Dot = getSelfLeaf match {
    case Some(leaf) => leaf.advance
    case None       => Dot(selfUid, 0)
  }

  override def toString: String = s"${tree}"
  def toTreeLikeString: String  = tree.toTreeLikeString

  /** update the latest dot emitted here known by the peer
    *
    * @param peerUid         the uid of the peer
    * @param lastKnownDots   the latest known dot of all peers from the peer
    * @return true if the peer is up to date
    */
  def updateKnowledgeOfPeer(peerUid: Uid, lastKnownDots: Dots): Unit = {
    var updatedKnowledge = false
    lastKnownDots.peers.foreach { knownPeerUid =>
      val lastKnownDot = lastKnownDots.clockOf(knownPeerUid)
      if tree.updateKnowledgeOfPeer(peerUid, lastKnownDot.get) then updatedKnowledge = true
    }

    if updatedKnowledge then tree.collapseGeneralKnowledge()
  }

  def getSelfKnowledge: Dots = tree.getCausalBarriersAsDots

  def getPredecessors(dot: Dot): List[Dot] = tree.getNodeOfDot(dot) match {
    case Some(node: DotTreeNode.Node) => tree.getPredecessors(node.predecessorNode, List.empty)
    case _                            => List.empty
  }

  def getPredecessors(uid: Uid): List[Dot] = tree.getLeaf(uid) match {
    case Some(node) => tree.getPredecessors(node, List.empty)
    case None       => List.empty
  }

}
