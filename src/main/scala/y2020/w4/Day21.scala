package y2020.w4

import common.Day
import common.Utils._

import scala.io.Source

class Day21 extends Day(inputPath(2020, 21), testPath(2020, 21, 1)) {
  private val foodStrings: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val foods: Array[Array[Set[String]]] = foodStrings.map(foodStr => {
    val parts = foodStr.split("""\(contains """)
    Array(parts(0).init.split(" ").toSet, parts(1).init.split(", ").toSet)
  })
  private var possibleIngredients = Map.empty[String, Set[String]]
  private val safes = foods.map(_(0))
  for (i <- foods.indices) {
    for (allergen <- foods(i)(1)) {
      val foodIndicesWithAllergen = foods.indices.filter(j => j != i && foods(j)(1).contains(allergen))
      val possibles = foodIndicesWithAllergen.map(foods(_)(0)).foldLeft(foods(i)(0))(_ intersect _)
      possibleIngredients += ((allergen, possibles))
      safes.indices.foreach(j => safes(j) = safes(j) -- possibles)
    }
  }

  def one: Int = safes.flatten.length

  while (possibleIngredients.exists(_._2.size > 1)) {
    val decided = possibleIngredients.filter(_._2.size == 1).values.reduce(_ union _)
    possibleIngredients = possibleIngredients.view.mapValues(possibles =>
      if (possibles.size > 1) possibles -- decided else possibles).toMap
  }

  def two: Unit = {
    val keys = possibleIngredients.keys.toArray.sorted
    println(keys.map(possibleIngredients(_).head).mkString(","))
  }
}