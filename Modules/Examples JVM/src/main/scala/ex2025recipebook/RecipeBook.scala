package ex2025recipebook

import com.softwaremill.quicklens.*
import ex2025recipebook.RecipeBook.Delta
import rdts.base.Historized.MetaDelta
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.ObserveRemoveMap
import rdts.time.Dots

case class RecipeBook(
    recipes: ObserveRemoveMap[String, Recipe]
) {

  def addRecipe(key: String, recipe: Recipe)(using localUid: LocalUid): Delta =
    this.deltaModify(_.recipes).using(_.update(key, recipe))

  def deleteRecipe(key: String): Delta =
    this.deltaModify(_.recipes).using(_.remove(key))

  def updateRecipeTitle(key: String, updatedRecipeTitle: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(key) {
        case Some(prior) => Some(prior.deltaModify(_.title).using(_.write(updatedRecipeTitle)))
        case None        => ???
      }
    }
  }

  def addIngredient(recipeKey: String, ingredient: Ingredient)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(recipeKey) {
        case Some(prior) => Some(prior.deltaModify(_.ingredients).using(_.append(ingredient)))
        case None        => ???
      }
    }
  }

  def updateIngredient(recipeKey: String, ingredientIndex: Int, mod: (Ingredient) => Ingredient)(using
      localUid: LocalUid
  ): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(recipeKey) {
        case Some(prior) => Some(prior.deltaModify(_.ingredients).using(_.update(ingredientIndex, mod)))
        case None        => ???
      }
    }
  }

  override def toString: String =
    f"[${this.recipes.inner.map(_._2.value).mkString(", ")}]"

}

object RecipeBook {

  type Delta = RecipeBook

  given Bottom[RecipeBook] = Bottom.derived

  given Lattice[RecipeBook] = Lattice.derived

  given Historized[RecipeBook] = Historized.productHistorized

  def apply(): RecipeBook = RecipeBook(ObserveRemoveMap.empty)

  def main(args: Array[String]): Unit = {
    val replica1: Replica[RecipeBook, DeltaBufferNonRedundant[RecipeBook]] =
      Replica(DeltaBufferNonRedundant[RecipeBook](List.empty[MetaDelta[RecipeBook]], Dots.empty))

    println("--- add recipe")
    val recipe1 = Recipe(
      "Piza",
      List(Ingredient("Teig", 1.0, "Stk."), Ingredient("Tomaten", 1.0, "Dose"), Ingredient("Mozzarella", 1.0, "Pkg"))
    )(using replica1.replicaId)
    replica1.mod(recipeBook => recipeBook.addRecipe("pizza", recipe1))
    println("---")

    println("--- update title to pizza")
    replica1.mod(recipeBook => recipeBook.updateRecipeTitle("pizza", "Pizza"))
    println("---")

    println("--- update title to pizzza")
    replica1.mod(recipeBook => recipeBook.updateRecipeTitle("pizza", "Pizzza"))
    println("---")

    println("--- add another recipe")
    val recipe2 = Recipe(
      "Pasta",
      List(Ingredient("Pasta", 200.0, "g"), Ingredient("Tomaten", 1.0, "Dose"), Ingredient("Parmesan", 10.0, "g"))
    )(using replica1.replicaId)
    replica1.mod(recipeBook => recipeBook.addRecipe("pasta", recipe2))
    println("---")

    println("--- remove first recipe")
    replica1.mod(recipeBook => recipeBook.deleteRecipe("pizza"))
    println("---")

    println("--- add again first recipe")
    replica1.mod(recipeBook => recipeBook.addRecipe("pizza", recipe1))
    println("---")

  }

}
