package ex2026darelecture

import rdts.base.Lattice

object Lecture {

  case class Product[A, B, C](a: A, b: B, c: C)

  enum Sum:
      case D
      case E(x: Int, y: Int)
      case F

  given [A: Lattice, B: Lattice, C: Lattice] => Lattice[Product[A, B, C]] = Lattice.derived

  given Lattice[Sum] =
      given Lattice[Sum.D.type] = Lattice.derived

      given Lattice[Sum.E] = Lattice.derived
      given Lattice[Int]   = Lattice.fromOrdering

      given Lattice[Sum.F.type] = Lattice.derived

      Lattice.sumLattice
}
