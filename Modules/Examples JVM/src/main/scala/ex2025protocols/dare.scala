package ex2025protocols

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.{GrowOnlyCounter, PosNegCounter}
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer}

class Replica[A](init: A) {
  val replicaId: LocalUid             = LocalUid.gen()
  val buffer: DeltaBufferContainer[A] = DeltaBuffer(init).mutable

  def mod(f: LocalUid ?=> A => A)(using Lattice[A]): this.type = {
    buffer.mod(f(using replicaId))
    this
  }

  def receive(other: Replica[A])(using Lattice[A]): this.type = {
    buffer.applyDelta(other.buffer.result.state)
    this
  }

  def show[B](select: A => B = identity): Unit = {
    print(s"$replicaId: ")
    pprint.pprintln(select(buffer.result.state))
  }

}

object Replica {
  def quiescence[A: Lattice](replicas: Replica[A]*): Unit =
    replicas.toList match
      case Seq() | Seq(_) => ()
      case Seq(a, rem*)   =>
        rem.foreach(a.receive)
        rem.foreach(r => r.receive(a))

}

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.LastWriterWins

object MiniSocial {

  given Lattice[MiniSocial] = Lattice.derived

  // needed by the replication manager to handle some special cases
  given bottom: Bottom[MiniSocial] = Bottom.provide(MiniSocial())

}

case class MiniSocial(
    message: LastWriterWins[String] = LastWriterWins.fallback(""),
    upvotes: GrowOnlyCounter = GrowOnlyCounter.zero,
    downvotes: GrowOnlyCounter = GrowOnlyCounter.zero
) {
  def like()(using LocalUid): MiniSocial =
    MiniSocial(upvotes = upvotes.add(1))

  def dislike()(using LocalUid): MiniSocial =
    MiniSocial(downvotes = downvotes.add(1))

  def setMessage(newMessage: String): MiniSocial =
    MiniSocial(message = message.write(newMessage))
}

object Dare {

  def main(args: Array[String]): Unit = {
//    {
//      val init = PosNegCounter.zero
//      // this means a, b, and c are all initilized by executing the RHS multiple times
//      val a, b, c = Replica(init)
//
//      a.mod(_.inc())
//      b.mod(_.inc())
//      c.receive(a).receive(b)
//      a.show()
//      b.show()
//      c.show()
//
//    }
//
//    {
//      val a, b, c = Replica(MiniSocial())
//
//      a.mod(_.setMessage("Welcome To DARE"))
//      b.mod(_.like())
//      c.mod(_.like())
//      Replica.quiescence(a, b, c)
//
//      a.show()
//      b.show()
//      c.show()
//    }

    {
      val a, b, c = Replica(MiniSocial())

      a.mod(_.setMessage("Welcome To DARE"))
      b.mod(_.like())
      c.mod(_.like())
      Replica.quiescence(a, b, c)

      a.show()
      b.show()
      c.show()
    }

  }

}
