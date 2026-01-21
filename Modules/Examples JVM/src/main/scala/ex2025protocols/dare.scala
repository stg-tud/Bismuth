//> using scala 3.7.2
//> using dep "de.tu-darmstadt.stg::rdts:0.37.0+651-afc47005"
//> using dep "com.lihaoyi::pprint:0.9.3"

package ex2025protocols

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.{GrowOnlyCounter, LastWriterWins}
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer}

/** For this lecuter/exercise, we are going to explore an executable model of a distributed system,
  * consisting of many of these replicas. Each replica has its own current state, and a replica ID.
  * There is no actual network involved, and it is just a helper
  * to make exploring possible behaviour of a distributed system possible.
  * It has no value in a real program.
  */
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

/** Mini Social models a social network … except that there is only a single global message which can be voted for.
  * Also when changing the message, the votes stay, so be careful.
  * Its not a useful thing, but it does model the potential of composition.
  */
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

object MiniSocial {
  given Lattice[MiniSocial] = Lattice.derived
}

/** The main objct, serving as kinda tests to play around with. */
object Dare {

  def main(args: Array[String]): Unit = {
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
