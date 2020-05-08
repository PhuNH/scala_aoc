package y2019.w3

import common.Day
import common.Utils._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// 1: 13312, 82892753
// 3: 2210736, 460664
class Day14 extends Day(inputPath(2019, 14),
  testPath(2019, 14, 1), testPath(2019, 14, 3)) {
  type RList = Array[Array[Array[String]]]
  type Component = (Chemical, Long)
  type Components = Array[Component]
  private val oreName = "ORE"
  private val fuelName = "FUEL"

  case class Chemical(name: String, quantity: Int, components: Components, level: Int)
  object Ore extends Chemical(oreName, 1, Array(), 0)

  private def getReactionList: RList = using(Source.fromFile(inputs(0)))(
    _.getLines().toArray.map(l => {
      l.split(",=>".toArray).filterNot(_ == "").map(_.trim.split(' '))
    })
  )

  private val reactionList = getReactionList

  private def maxLevel(components: Components): Int = components.map(_._1.level).max

  private val chemList = ArrayBuffer.empty[Chemical]

  private def defineChemical(reactionList: RList, name: String): Chemical = {
    if (name == oreName) Ore
    else {
      chemList.find(_.name == name) match {
        case Some(chem) => chem
        case _ => reactionList.filter(_.last(1) == name) match {
          case result if result.length != 1 => throw new IllegalArgumentException("Chemical name not found")
          case result =>
            val line = result.head
            val quantity = line.last(0).toInt
            val components = line.init.map(chem => (defineChemical(reactionList, chem(1)), chem(0).toLong))
            val newChem = Chemical(name, quantity, components, maxLevel(components) + 1)
            chemList.addOne(newChem)
            newChem
        }
      }
    }
  }

  @scala.annotation.tailrec
  private def recCalOre(components: Components, residuals: ArrayBuffer[Component]): Long = {
    val level = maxLevel(components)
    if (level == 0) components.head._2
    else {
      val (thisLevel, rest) = components.partition(_._1.level == level)
      val thisLevelSubtracted = thisLevel.map(comp => residuals.indexWhere(_._1 == comp._1) match {
        case i if i > -1 =>
          val diff = comp._2 - residuals(i)._2
          if (diff < 0) residuals.update(i, (residuals(i)._1, -diff))
          else residuals.subtractOne(residuals(i))
          (comp._1, diff)
        case _ => comp
      }).filter(_._2 > 0)
      val below = thisLevelSubtracted.flatMap(comp => {
        val multi = (comp._2 - 1) / comp._1.quantity + 1
        val residualQuantity = multi * comp._1.quantity - comp._2
        if (residualQuantity > 0) residuals.addOne((comp._1, residualQuantity))
        comp._1.components.map(c => (c._1, c._2 * multi))
      })

      val mergedBelowAndRest = (below ++ rest).groupBy(_._1).map(g => (g._1, g._2.map(_._2).sum)).toArray
      recCalOre(mergedBelowAndRest, residuals)
    }
  }

  private def calculateOre(chem: Chemical, residuals: ArrayBuffer[Component]): Long =
    recCalOre(chem.components, residuals)

  def one: Long = {
    val fuel = defineChemical(reactionList, fuelName)
    val residuals = ArrayBuffer.empty[Component]
    calculateOre(fuel, residuals)
  }

  @scala.annotation.tailrec
  private def recCalFuel(components: Components, residuals: ArrayBuffer[Component],
                         ores: Long, fuelAcc: Long): Long = {
    if (ores == 0) fuelAcc
    else if (ores < 0) fuelAcc - 1
    else {
      val requiredOres = recCalOre(components, residuals)
      recCalFuel(components, residuals, ores - requiredOres, fuelAcc+1)
    }
  }

  private val maxOres = 1000000000000L
  private val factor = 100

  private def calOreForFuel(fuelNum: Long, components: Components): Long = {
    val componentsOfNum = components.map(c => (c._1, c._2 * fuelNum))
    recCalOre(componentsOfNum, ArrayBuffer.empty)
  }

  @scala.annotation.tailrec
  private def firstHigherNumberFuel(components: Components, fuelNum: Long): Long = {
    val oreNum = calOreForFuel(fuelNum, components)
    if (oreNum > maxOres) fuelNum
    else firstHigherNumberFuel(components, fuelNum * factor)
  }

  @scala.annotation.tailrec
  private def binarySearchNumberFuel(components: Components, lower: Long, upper: Long): Long = {
    if (upper == lower + 1) lower
    else {
      val middle = (upper + lower) / 2
      val oreNum = calOreForFuel(middle, components)
      if (oreNum >= maxOres) binarySearchNumberFuel(components, lower, middle)
      else binarySearchNumberFuel(components, middle, upper)
    }
  }

  def two: Long = {
    val fuel = defineChemical(reactionList, fuelName)
    //val residuals = ArrayBuffer.empty[Component]
    //recCalFuel(fuel.components, residuals, maxOres, 0)

    val firstUpper = firstHigherNumberFuel(fuel.components, 1)
    val firstLower = firstUpper / factor
    binarySearchNumberFuel(fuel.components, firstLower, firstUpper)
  }
}