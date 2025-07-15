package replication

import rdts.base.Uid
import rdts.time.{Dot, Dots}
import replication.ProtocolMessage.Payload


class DeltaTreeContext[State](selfId: Uid) {

  // (replicaId, parent, successor)
  private var predecessors: Map[Dot, Dot] = Map.empty
  // dots without successor
  private var leaves: Map[Uid, Dot] = Map.empty

  def getPredecessors(dot: Dot): List[Dot] = predecessors.get(dot) match {
    case Some(value) => value :: getPredecessors(value)
    case None => List.empty
  }

  def getPredecessors(uid: Uid): List[Dot] = leaves.get(uid) match {
    case Some(leaf) => leaf :: getPredecessors(leaf)
    case None => List.empty
  }

  def getLeaf(uid: Uid): Option[Dot] = leaves.get(uid) match {
    case Some(leaf) => Option.apply(leaf)
    case None => Option.empty
  }

  def getSelfLeaf: Option[Dot] = leaves.get(selfId) match {
    case Some(leaf) => Option.apply(leaf)
    case None => Option.empty
  }

  def findSuccessor(dot: Dot, start: Dot): Option[Dot] = predecessors.get(start) match {
    case Some(n) => if (n == dot) then Some(start) else findSuccessor(dot, n)
    case None => None
  }

  /**
   *
   * @param dot
   * @param start
   * @return (parent, successor) parent is optional
   */
  def findPlace(dot: Dot, start: Dot): (Option[Dot], Dot) = predecessors.get(start) match {
    case Some(predecessor) => {
      if predecessor == dot then {
        (Option.apply(predecessor), start)
      } else {
        findPlace(dot, predecessor)
      }
    }
    case None => (Option.empty, start)
  }

  /**
   *
   * @param dot
   * @param predecessor
   * @return true if dot was added as a leaf to the tree structure and false otherwise
   */
  def addNode(dot: Dot, predecessor: Option[Dot]): Boolean = {
    getLeaf(dot.place) match {
      case Some(leaf) => {
        predecessor match {
          case Some(predecess) => {
            predecessors += (dot -> predecess)
            leaf.time compare predecess.time match {
              case -1 => {
                leaves += (dot.place -> dot)
                predecessors += (dot -> leaf)
                true
              }
              case 0 => false
              case 1 => {
                // find predecessor of predecessor
                val (p, s) = findPlace(predecess, leaf)
                p match {
                  case Some(value) => predecessors += (predecess -> value)
                  case None => {}
                }
                predecessors += (s -> dot)
                false
              }
            }
          }
          case None => {
            leaf.time compare dot.time match {
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
                  case None => {}
                }
                predecessors += (s -> dot)
                false
              }
            }
          }
        }
      }
      case None => {
        leaves += (dot.place -> dot)
        predecessor match {
          case Some(predecess) => predecessors += (dot -> predecess)
          case None => {}
        }
        true
      }
    }
  }

  def getNonRedundant(dots: Dots): Dots = {
    dots.internal.foldLeft(Dots.empty) { case (acc, (uid, range)) =>
      range.inner.foldLeft(acc) { (currentDots, time) =>
        val dot = Dot(uid, time)
        if addNode(dot, None) then currentDots.add(dot) else currentDots
      }
    }
  }

}
