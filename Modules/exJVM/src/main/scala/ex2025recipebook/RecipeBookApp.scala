package ex2025recipebook

import rdts.base.{LocalUid, Uid}

object RecipeBookApp {

  def main(args: Array[String]): Unit = {
    val localUid1, localUid2     = LocalUid(Uid.gen())
    var recipeBook1, recipeBook2 = RecipeBook()

    def printRecipes(): Unit = {
      println("---")
      println(f"$localUid1: $recipeBook1")
      println(f"$localUid2: $recipeBook2")
    }

    printRecipes()

    var pizzaRecipe = Recipe("Pizza")
    val delta1      = recipeBook1.addRecipe("pizza", pizzaRecipe)(using localUid1)
    recipeBook1 = recipeBook1.merge(delta1)

    printRecipes()

    recipeBook2 = recipeBook2.merge(delta1)

    printRecipes()

    var pastaRecipe = Recipe("Pasta")
    val delta2      = recipeBook2.addRecipe("pasta", pastaRecipe)(using localUid2)
    recipeBook2 = recipeBook2.merge(delta2)
    // pizzaRecipe = pizzaRecipe.merge(pizzaRecipe.addIngredient(Ingredient("Teig", 1.0, "Stk."))(using localUid1))
    // val delta3 = recipeBook1.updateRecipe("pizza", pizzaRecipe)(using localUid1)
    val delta3 = recipeBook1.addIngredient("pizza", Ingredient("Teig", 1.0, "Stk."))(using localUid1)
    recipeBook1 = recipeBook1.merge(delta3)

    printRecipes()

    recipeBook1 = recipeBook1.merge(delta2)
    recipeBook2 = recipeBook2.merge(delta3)

    printRecipes()
    println(recipeBook1.subsumes(recipeBook2))

    val delta4 = recipeBook1.addIngredient("pizza", Ingredient("Tomaten", 1.0, "Dose"))(using localUid1)
    val delta5 = recipeBook2.addIngredient("pizza", Ingredient("Mozzarella", 1.0, "Packung"))(using localUid2)
    recipeBook1 = recipeBook1.merge(delta4).merge(delta5)
//    recipeBook2 = recipeBook2.merge(delta5).merge(delta4)

    printRecipes()

    println(f"delta 4: ${delta4}")
    println(f"delta 5: ${delta5}")
//    println(delta4 `merge` delta5)

    recipeBook2 = recipeBook2.merge(delta5)
    val delta6 = recipeBook2.addIngredient("pizza", Ingredient("Tomaten", 1.0, "Dose"))(using localUid2)
    recipeBook2 = recipeBook2.merge(delta6)

    printRecipes()

    println(f"delta 6: $delta6")
    println(delta4 `subsumes` delta6)
    println(recipeBook1 `subsumes` recipeBook2)

    recipeBook2 = recipeBook2.merge(delta4)
    recipeBook1 = recipeBook1.merge(delta5)

    println(recipeBook1 `subsumes` recipeBook2)

    recipeBook1 = recipeBook1.merge(delta6)

    println(recipeBook1 `subsumes` recipeBook2)

    printRecipes()

    println("\n---")

    delta5.merge(delta6)

    println("\n---")

    recipeBook1 = recipeBook1.merge(delta6)

    println("updating an ingredient")

//    val delta7 = recipeBook1.updateIngredient("pizza", 0, Ingredient("Teig", 1.0, "Stück"))(using localUid1)
//    recipeBook1 = recipeBook1.merge(delta7)
//    recipeBook2 = recipeBook2.merge(delta7)

    printRecipes()

  }

}
