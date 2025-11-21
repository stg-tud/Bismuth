package ex2025recipebook

import com.softwaremill.quicklens.*
import ex2025recipebook.RecipeBook.Delta
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.ObserveRemoveMap

case class RecipeBook(
    recipes: ObserveRemoveMap[String, Recipe]
) {

  def keys: Set[String] = recipes.inner.keySet

  def get(recipeKey: String): Option[Recipe] = recipes.inner.get(recipeKey).map(_.value)

  def addRecipe(key: String, recipe: Recipe)(using localUid: LocalUid): Delta =
    this.deltaModify(_.recipes).using(_.update(key, recipe))

  def deleteRecipe(key: String): Delta =
    this.deltaModify(_.recipes).using(_.remove(key))

  def updateRecipeTitle(recipeKey: String, updatedRecipeTitle: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(recipeKey) {
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

  def deleteIngredient(recipeKey: String, ingredientIndex: Int)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(recipeKey) {
        case Some(prior) => Some(prior.deltaModify(_.ingredients).using(_.remove(ingredientIndex)))
        case None => ???
      }
    }
  }

  def updateServings(recipeKey: String, newServings: Int)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(recipeKey) {
        case Some(prior) => Some(prior.updateServings(newServings))
        case None => ???
      }
    }
  }

  def updateCookingTime(recipeKey: String, newCookingTime: Int)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(recipeKey) {
        case Some(prior) => Some(prior.updateCookingTime(newCookingTime))
        case None => ???
      }
    }
  }

  def updateDescription(recipeKey: String, newDescription: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(recipeKey) {
        case Some(prior) => Some(prior.updateDescription(newDescription))
        case None => ???
      }
    }
  }

  def updateFavorite(recipeKey: String, newValue: Boolean)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.recipes).using { ormap =>
      ormap.transform(recipeKey) {
        case Some(prior) => Some(prior.deltaModify(_.favorite).using(ew => if newValue then ew.enable(using localUid)() else ew.disable()))
        case None => ???
      }
    }
  }

  def isEmpty: Boolean = recipes.inner.isEmpty

  def nonEmpty: Boolean = !isEmpty

  override def toString: String =
    f"[${this.recipes.inner.map(_._2.value).mkString(", ")}]"

}

object RecipeBook {

  type Delta = RecipeBook

  val empty: RecipeBook = RecipeBook(ObserveRemoveMap.empty)

  given Bottom[RecipeBook] = Bottom.derived

  given Lattice[RecipeBook] = Lattice.derived

  given Historized[RecipeBook] = Historized.productHistorized

  def apply(): RecipeBook = RecipeBook(ObserveRemoveMap.empty)

  def main(args: Array[String]): Unit = {
    val replica1: Replica[RecipeBook, DeltaBufferNonRedundant[RecipeBook]] = Replica(LocalUid.gen(), RecipeBook.empty, DeltaBufferNonRedundant[RecipeBook]())

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
