package ex2025recipebook

import com.softwaremill.quicklens.*
import ex2025recipebook.Recipe.Delta
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, LastWriterWins}

case class Recipe(
    title: LastWriterWins[String],
    ingredients: NestedKeepRemoveList[Ingredient],
    servings: LastWriterWins[Int],
    cookingTime: LastWriterWins[Int],
    description: LastWriterWins[String],
    favorite: EnableWinsFlag
) {

  private inline def mod[A](inline path: Delta => A, mod: A => A): Delta =
    this.deltaModify(path).using(mod)

  def editTitle(newTitle: String): Delta =
    mod(_.title, _.write(newTitle))

  def addIngredient(newIngredient: Ingredient)(using localUid: LocalUid): Delta =
    mod(_.ingredients, _.append(newIngredient))

  def updateIngredient(index: Int, modify: (Ingredient) => Ingredient)(using localUid: LocalUid): Delta =
    mod(_.ingredients, _.update(index, modify))

  def removeIngredient(index: Int): Delta =
    mod(_.ingredients, _.remove(index))

  def updateServings(newServings: Int): Delta = mod(_.servings, _.write(newServings))

  def updateCookingTime(newCookingTime: Int): Delta = mod(_.cookingTime, _.write(newCookingTime))

  def updateDescription(newDescription: String): Delta = mod(_.description, _.write(newDescription))

  def setFavorite(value: Boolean)(using localUid: LocalUid): Delta =
    if value then mod(_.favorite, _.enable()) else mod(_.favorite, _.disable())

  override def toString: String = {
    val star = if favorite.read then "* " else ""
    f"$star${this.title.payload}(${this.ingredients.payloads.map(_._2.toString).mkString(", ")})"
  }

}

object Recipe {

  type Delta = Recipe

  val empty: Recipe = Recipe(LastWriterWins.empty[String], NestedKeepRemoveList.empty, LastWriterWins.empty, LastWriterWins.empty, LastWriterWins.empty, EnableWinsFlag.empty)

  given Bottom[String] = Bottom.provide("")

  given Bottom[Int] = Bottom.provide(0)

  given bottom: Bottom[Recipe] = Bottom.derived

  given Lattice[Recipe] = Lattice.derived

  given Historized[Recipe] = Historized.productHistorized

  def apply(title: String): Recipe = Recipe(LastWriterWins.empty[String].write(title), NestedKeepRemoveList.empty, LastWriterWins.empty, LastWriterWins.empty, LastWriterWins.empty, EnableWinsFlag.empty)

  def apply(title: String, ingredient: Ingredient)(using localUid: LocalUid): Recipe =
    Recipe(LastWriterWins.empty[String].write(title), NestedKeepRemoveList.empty[Ingredient].append(ingredient), LastWriterWins.empty, LastWriterWins.empty, LastWriterWins.empty, EnableWinsFlag.empty)

  def apply(title: String, ingredients: Iterable[Ingredient])(using localUid: LocalUid): Recipe =
    Recipe(LastWriterWins.empty[String].write(title), NestedKeepRemoveList.empty[Ingredient].appendAll(ingredients), LastWriterWins.empty, LastWriterWins.empty, LastWriterWins.empty, EnableWinsFlag.empty)

  def main(args: Array[String]): Unit = {
    val replica1, replica2: Replica[Recipe, DeltaBufferNonRedundant[Recipe]] = Replica(LocalUid.gen(), Recipe.empty, DeltaBufferNonRedundant[Recipe]())

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

    replica1.show()
    replica2.show()
  }

}
