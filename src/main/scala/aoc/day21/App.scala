package aoc.day21

import aoc.shared.Reader

case class Recipe(ingredients: List[String], allergens: List[String])

object App {
  def main(args: Array[String]): Unit = {

   val input = Reader.lines("21.txt")

    val recipeList = input.map(parse)

    val allergens = recipeList.flatMap(r => r.allergens.map(a => (a, r.ingredients)))
    val ingredients = recipeList.flatMap(r => r.ingredients.map(i => (i, r.allergens)))

    val groupedAllergens = allergens.groupBy(_._1).mapValues(_.flatMap(_._2).distinct).toMap
    val groupedIngredients = ingredients.groupBy(_._1).mapValues(_.flatMap(_._2).distinct).toMap

    val ingredientsWithoutAllergens = groupedIngredients.filter(m => {
      m._2.forall(a =>
        recipeList.exists(r => r.allergens.contains(a) && !r.ingredients.contains(m._1))
      )
    }).map(_._1).toList

    val part1 = ingredientsWithoutAllergens.map(s => recipeList.count(r => r.ingredients.contains(s))).sum
    println(part1)

    val filteredIngredients = groupedIngredients.filterNot(i => ingredientsWithoutAllergens.contains(i._1))
    val filteredRecipeList = recipeList

    val allAllergens = groupedAllergens.keys.toList.sorted
    val rules = filteredRecipeList.flatMap(r => r.allergens.map(a => (a, r.ingredients)))
    val allPossibleIngredients = filteredIngredients.map(_._1).toList.permutations.filter(_.size == allAllergens.size).filter( iList => {
      val zipped = allAllergens.zip(iList).toMap
      rules.forall( r => r._2.contains(zipped.get(r._1).getOrElse(None)))
    }).toList

    println(allPossibleIngredients.head.mkString(","))
 }

  val RecipePattern = "^(.*) \\(contains (.*)\\)$".r
  def parse(s: String): Recipe = {
    s match {
      case RecipePattern(iList, aList) =>
        val ingredients = iList.split(" ").toList
        val allergens = aList.replaceAll(",","").split(" ").toList
        Recipe(ingredients, allergens)
    }
  }
}