package ex2026darelecture

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.{GrowOnlyCounter, LastWriterWins}

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

/** The main object, serving as kinda tests to play around with. */
object MinisocialMain {

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
