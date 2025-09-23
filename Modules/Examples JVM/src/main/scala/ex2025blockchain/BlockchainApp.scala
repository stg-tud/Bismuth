package ex2025blockchain

import rdts.base.{Bottom, Lattice}


object BlockchainApp {

  given Lattice[String] = (a, b) => { if a.compareTo(b) >= 0 then a else b }

  given Bottom[String] = Bottom.provide("")

  def main(args: Array[String]): Unit = {
    val a, b, c = Replica(Blockchain.empty[String, String])

    a.mod(_.addBlock("a", "Welcome To DARE"))
    b.mod(_.addBlock("b", "Hello World"))
    c.mod(_.addBlock("c", "Goodbye World"))
    Replica.quiescence(a, b, c)

    a.show()
    b.show()
    c.show()

    println("---")

    b.mod(_.addBlock("e", "Hello again"))
    b.mod(_.addBlock("f", "Hello again again"))
    c.mod(_.addBlock("d", "Goodbye again"))
    Replica.quiescence(b, c)

    a.show()
    b.show()
    c.show()

    println("---")

    a.mod(_.addBlock("g", "Welcome again"))
    Replica.quiescence(a, b, c)

    a.show()
    b.show()
    c.show()

  }

}
