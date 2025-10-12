package ex2025blockchain

import rdts.base.{Bottom, Lattice}
import rdts.time.Dot

object BlockchainApp {

  given Lattice[String] = Lattice.fromOrdering

  given Bottom[String] = Bottom.provide("")

  def main(args: Array[String]): Unit = {
    val a, b, c = Replica(PoWBlockchain(Block("", None, "", Dot.zero)))

    a.mod(_.addBlock("a", "Welcome To DARE", a.nextDot))
    b.mod(_.addBlock("b", "Hello World", b.nextDot))
    c.mod(_.addBlock("c", "Goodbye World", c.nextDot))
    Replica.quiescence(a, b, c)

    a.show()
    b.show()
    c.show()

    println("---")

    b.mod(_.addBlock("e", "Hello again", b.nextDot))
    b.mod(_.addBlock("f", "Hello again again", b.nextDot))
    c.mod(_.addBlock("d", "Goodbye again", c.nextDot))
    Replica.quiescence(b, c)

    a.show()
    b.show()
    c.show()

    println("---")

    a.mod(_.addBlock("g", "Welcome again", a.nextDot))
    Replica.quiescence(a, b, c)

    a.show()
    b.show()
    c.show()

  }

}
