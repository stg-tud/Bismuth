package rdts.experiments

import rdts.base.{Lattice, LocalUid, Uid}

import scala.annotation.tailrec

type Hash = Int
type Path = String

trait Operation { def predecessors: Set[Hash] }
case class Delegation(delegator: Uid, delegatee: Uid, path: Path, predecessors: Set[Hash]) extends Operation
case class Root(owner: Uid)                                                                extends Operation {
  override def predecessors: Set[Hash] = Set.empty
}

object Operation {
  extension (op: Operation) def hash: Hash = op.hashCode()
}

case class HashDAGAcl(operations: Map[Hash, Operation]) {

  def delegate(p: Path, to: Uid)(using LocalUid): HashDAGAcl =
    operations.collectFirst {
      case (hash, Delegation(delegatee = delegatee, path = parentPath))
          if delegatee == LocalUid.replicaId && parentPath.startsWith(p) => hash
    } match {
      case None             => HashDAGAcl.empty
      case Some(parentHash) =>
        val op = Delegation(LocalUid.replicaId, to, p, Set(parentHash))
        HashDAGAcl(Map(op.hash -> op))
    }

  lazy val valid: Set[Operation] = {
    val successors =
      operations.values.flatMap(op => op.predecessors.map(p => (predecessor = p, op = op)))
        .groupBy(_.predecessor).view.mapValues(_.map(_.op)).toMap

    val init: Set[Operation] = operations.collect { case (h, root: Root) => root }.toSet

    @tailrec
    def fixpoint(current: Set[Operation], visited: Set[Operation]): Set[Operation] = {
      val newOps = current.flatMap { op =>
        successors.getOrElse(op.hash, Set.empty).collect {
          case d: Delegation if op match {
                case Root(_)                         => true
                case Delegation(_, _, parentPath, _) => d.path.contains(parentPath)
              } => d
        }
      }.diff(visited)

      if newOps.isEmpty then visited
      else fixpoint(newOps.toSet, visited ++ newOps)
    }

    fixpoint(init, init)
  }
}

object HashDAGAcl {
  def empty                          = HashDAGAcl(Map.empty)
  given lattice: Lattice[HashDAGAcl] =
      given Lattice[Operation] = Lattice.assertEquals
      Lattice.derived
}
