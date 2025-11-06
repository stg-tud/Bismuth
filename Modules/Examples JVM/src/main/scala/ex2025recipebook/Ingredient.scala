package ex2025recipebook

import com.softwaremill.quicklens.*
import ex2025recipebook.Ingredient.{Delta, stringBottom}
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.LastWriterWins

/** complicated on purpose for more nested ardts
  *
  * @param name   the ingredient
  * @param amount how much of that ingredient, more spices are always good :)
  * @param unit   only metric units allowed, other units might have unexpected side effects ;)
  */
case class Ingredient(
    name: LastWriterWins[String],
    amount: LastWriterWins[Double],
    unit: LastWriterWins[String],
) {

  def updateName(newName: String): Delta =
    this.deltaModify(_.name).using(_.write(newName))

  def updateAmount(newAmount: Double): Delta =
    this.deltaModify(_.amount).using(_.write(newAmount))

  def updateUnit(newUnit: String): Delta =
    this.deltaModify(_.unit).using(_.write(newUnit))

  override def toString: String = f"${amount.payload} ${unit.payload} ${name.payload}"

}

object Ingredient {

  type Delta = Ingredient

  val empty: Ingredient = Ingredient(LastWriterWins.empty[String], LastWriterWins.empty[Double], LastWriterWins.empty[String])

  given stringBottom: Bottom[String] = Bottom.provide("")

  given doubleBottom: Bottom[Double] = Bottom.provide(0.0)

  given bottom: Bottom[Ingredient] = Bottom.derived

  given lattice: Lattice[Ingredient] = Lattice.derived

  given history: Historized[Ingredient] = Historized.productHistorized

  def apply(name: String, amount: Double, unit: String): Ingredient =
    Ingredient(
      LastWriterWins.empty[String].write(name),
      LastWriterWins.empty[Double].write(amount),
      LastWriterWins.empty[String].write(unit)
    )

  def main(args: Array[String]): Unit = {
    val replica1: Replica[Ingredient, DeltaBufferNonRedundant[Ingredient]] = Replica(LocalUid.gen(), Ingredient.empty, DeltaBufferNonRedundant[Ingredient]())
    def ingredient                    = replica1.state

    println("---0")
    val delta0 = Ingredient("Teig", 1.0, "Stk.")
    replica1.mod(_ => delta0)
    println("---")

    println("---1")
    val delta1 = ingredient.updateAmount(2.0)
    replica1.mod(_ => delta1)
    println("---")

    println("---2")
    val delta2 = ingredient.updateAmount(2.0)
    replica1.mod(_ => delta2)
    println("---")

    println("---3")
    val delta3 = ingredient.updateAmount(3.0)
    replica1.mod(_ => delta3)
    println("---")

    println("---4")
    val delta4 = ingredient.updateUnit("Stück")
    replica1.mod(_ => delta4)
    println("---")

    println("---5")
    val delta5 = ingredient.updateAmount(0.0)
    replica1.mod(_ => delta5)
    println("---")

    replica1.show()
    println(replica1.buffer.getSize)
  }
}
