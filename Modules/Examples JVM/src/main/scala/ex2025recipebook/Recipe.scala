package ex2025recipebook

import com.softwaremill.quicklens.*
import ex2025recipebook.Recipe.Delta
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, LastWriterWins}

case class Recipe(
    title: LastWriterWins[String],
    ingredients: NestedKeepRemoveList[Ingredient],
    favorite: EnableWinsFlag = EnableWinsFlag.empty,
    numberOfEdits: GrowOnlyCounter = GrowOnlyCounter.zero
) {

  private inline def mod[A](inline path: Delta => A, mod: A => A)(using localUid: LocalUid): Delta =
    this.deltaModify(path).using(mod) `merge` this.deltaModify(_.numberOfEdits).using(_.inc())

  def editTitle(newTitle: String)(using localUid: LocalUid): Delta =
    mod(_.title, _.write(newTitle))

  def addIngredient(newIngredient: Ingredient)(using localUid: LocalUid): Delta =
    mod(_.ingredients, _.append(newIngredient))

  def updateIngredient(index: Int, modify: (Ingredient) => Ingredient)(using localUid: LocalUid): Delta =
    mod(_.ingredients, _.update(index, modify))

  def removeIngredient(index: Int)(using localUid: LocalUid): Delta =
    mod(_.ingredients, _.remove(index))

  def setFavorite(value: Boolean)(using localUid: LocalUid): Delta =
    if value then mod(_.favorite, _.enable()) else mod(_.favorite, _.disable())

  override def toString: String = {
    val star = if favorite.read then "* " else ""
    f"$star${this.title.payload}(${numberOfEdits.value})(${this.ingredients.payloads.map(_._2.toString).mkString(", ")})"
  }

}

object Recipe {

  type Delta = Recipe

  given Bottom[String] = Bottom.provide("")

  given bottom: Bottom[Recipe] = Bottom.derived

  given Lattice[Recipe] = Lattice.derived

  given Historized[Recipe] = Historized.productHistorized

  def apply(title: String): Recipe = Recipe(LastWriterWins.empty[String].write(title), NestedKeepRemoveList.empty)

  def apply(title: String, ingredient: Ingredient)(using localUid: LocalUid): Recipe =
    Recipe(LastWriterWins.empty[String].write(title), NestedKeepRemoveList.empty[Ingredient].append(ingredient))

  def apply(title: String, ingredients: Iterable[Ingredient])(using localUid: LocalUid): Recipe =
    Recipe(LastWriterWins.empty[String].write(title), NestedKeepRemoveList.empty[Ingredient].appendAll(ingredients))

  def main(args: Array[String]): Unit = {
    val replica1: Replica[Recipe] = Replica()
    val replica2: Replica[Recipe] = Replica()

    println("---0")
    val delta0 = Recipe("Piza")
    replica1.mod(_ => delta0)
    Replica.quiescence(replica1, replica2)
    println("---")

    println("---1")
    replica1.mod(recipe => recipe.editTitle("Pizza"))
    Replica.quiescence(replica1, replica2)
    println("---")

    println("---2")
    replica1.mod(recipe => recipe.addIngredient(Ingredient("Teig", 1.0, "Stk.")))
    Replica.quiescence(replica1, replica2)
    println("---")

    println("---3")
    replica1.mod(recipe => recipe.updateIngredient(0, ingredient => ingredient.updateUnit("Stück")))
    Replica.quiescence(replica1, replica2)
    println("---")

    println("---4")
    replica1.mod(recipe => recipe.setFavorite(true))
    Replica.quiescence(replica1, replica2)
    println("---")

    println("---5")
    replica2.mod(recipe => recipe.setFavorite(false))
    Replica.quiescence(replica1, replica2)
    println("---")

    println("---6")
    replica2.mod(recipe => recipe.setFavorite(true))
    Replica.quiescence(replica1, replica2)
    println("---")

    println("fffffinal")
    println("redundant: replica2 -> [0]")
    println(f"replica1 (${replica1.replicaId}) redundant: ${replica1.buffer.result.redundantDots}")
    println(f"replica2 (${replica2.replicaId}) redundant: ${replica2.buffer.result.redundantDots}")
  }

}
