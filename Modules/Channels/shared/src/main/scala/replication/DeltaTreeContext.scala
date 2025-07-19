package replication

import rdts.base.Uid
import rdts.time.{ArrayRanges, Dot, Dots}
import replication.ProtocolMessage.Payload

import scala.annotation.tailrec

class DeltaTreeContext[State](selfUid: Uid) {

  // (parent, successor)
  private var predecessors: Map[Dot, Dot] = Map.empty
  // dots without successor
  private var leaves: Map[Uid, Dot]                         = Map.empty
  private var payloads: List[CachedMessage[Payload[State]]] = List.empty
  // look up table to index of cached messages
  private var dotToPayload: Map[Dot, List[Int]] = Map.empty

  def getPredecessors(dot: Dot): List[Dot] = predecessors.get(dot) match {
    case Some(value) => value :: getPredecessors(value)
    case None        => List.empty
  }

  /** @param uid replicaUid
    * @return list of predecessors of the current leaf of the replica, head is the leaf
    */
  def getPredecessors(uid: Uid): List[Dot] = leaves.get(uid) match {
    case Some(leaf) => leaf :: getPredecessors(leaf)
    case None       => List.empty
  }

  def getLeaf(uid: Uid): Option[Dot] = leaves.get(uid) match {
    case Some(leaf) => Option.apply(leaf)
    case None       => Option.empty
  }

  def getSelfLeaf: Option[Dot] = getLeaf(selfUid)

  def findSuccessor(dot: Dot, start: Dot): Option[Dot] = predecessors.get(start) match {
    case Some(n) => if n == dot then Some(start) else findSuccessor(dot, n)
    case None    => None
  }

  /**
    * @param dot the dot for whom to find the place in the tree structure
    * @param start more advanced in time than dot
    * @return (parent, successor) parent is optional
    */
  @tailrec
  private def findPlace(dot: Dot, start: Dot): (Option[Dot], Dot) = predecessors.get(start) match {
    case Some(predecessor) => {
      if predecessor.place != dot.place || predecessor.time < dot.time then {
        (Option(predecessor), start)
      } else {
        findPlace(dot, predecessor)
      }
    }
    case None => (Option.empty, start)
  }

  private def addNodeWithPredecessor(dot: Dot, predecessor: Dot): Boolean = getLeaf(dot.place) match {
    case Some(leaf) => {
      predecessors += (dot -> predecessor)
      leaf.time compare predecessor.time match {
        case -1 => false // TODO check if predecessor is in tree
        case 0  => {
          leaves += (dot.place -> dot)
          predecessors += (dot -> leaf)
          true
        }
        case 1 => {
          // find predecessor of predecessor
          val (p, s) = findPlace(predecessor, leaf)
          p match {
            case Some(value) => predecessors += (predecessor -> value)
            case None        => {}
          }
          predecessors += (s -> dot)
          false
        }
      }
    }
    case None => {
      leaves += (dot.place -> dot)
      predecessors += (dot -> predecessor)
      true
    }
  }

  private def addNodeWithoutPredecessor(dot: Dot): Boolean = getLeaf(dot.place) match {
    case Some(leaf) => leaf.time compare dot.time match {
        case -1 => {
          predecessors += (dot -> leaf)
          leaves += (dot.place -> dot)
          true
        }
        case 0 => false
        case 1 => {
          val (p, s) = findPlace(dot, leaf)
          p match {
            case Some(value) => predecessors += (dot -> value)
            case None        => {}
          }
          predecessors += (s -> dot)
          false
        }
      }
    case None => {
      leaves += (dot.place -> dot)
      true
    }
  }

  /** @param dot
    * @param predecessor
    * @return true if dot was added as a leaf to the tree structure and false otherwise
    */
  def addNode(dot: Dot, predecessor: Option[Dot] = Option.empty): Boolean = predecessor match {
    case Some(predecess) => addNodeWithPredecessor(dot, predecess)
    case None            => addNodeWithoutPredecessor(dot)
  }

  def getNonRedundant(dots: Dots): Dots = dots.iterator.foldLeft(Dots.empty) { (currentDots, dot) =>
    if addNode(dot) then currentDots.add(dot) else currentDots
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
    getSelfLeaf match {
      case Some(leaf) => predecessors += (dot -> leaf)
      case None       => {}
    }
    leaves += (selfUid -> dot)

    // store message
    payloads = payloads :+ message
    addDotToPayload(payloads.size - 1, dot)
  }

  private def addMissingDot(dot: Dot, known: Dots): Dots = {
    var newKnowledge = Dots.empty
    if !known.contains(dot) then newKnowledge = newKnowledge.add(dot)
    predecessors.get(dot) match {
      case Some(predecessor) => newKnowledge.merge(addMissingDot(predecessor, known))
      case None              => newKnowledge
    }
  }

  def getMissingDots(requesterUid: Uid, known: Dots): Dots = {
    val newKnowledge: Dots = Dots.empty

    leaves.filterNot { (uid, _) => uid == requesterUid }.foreach { (_, leaf) =>
      newKnowledge.merge(addMissingDot(leaf, known))
    }

    newKnowledge
  }

  def getPayloads(dots: Dots): List[CachedMessage[Payload[State]]] = {
    val indices = dotToPayload.filter((dot, _) => dots.contains(dot)).flatMap((_, msgs) => msgs).toSet
    payloads.zipWithIndex.collect { case (msgs, i) if indices.contains(i) => msgs }
  }

  def getAllPayloads: List[CachedMessage[Payload[State]]] = payloads

  def getSelfContext: Dots = getPredecessors(selfUid).foldRight(Dots.empty)((dot, dots) => dots.add(dot))

  def getNextDot: Dot = getSelfLeaf match {
    case Some(leaf) => leaf.advance
    case None       => Dot(selfUid, 0)
  }

  override def toString: String = s"leaves: ${leaves}\npredecessors: ${predecessors}"

}
