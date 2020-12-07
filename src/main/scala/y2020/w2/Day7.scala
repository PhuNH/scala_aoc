package y2020.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day7 extends Day(inputPath(2020, 7), testPath(2020, 7, 1)) {
  private val ruleStrings = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val splitted = ruleStrings.map(_.init.split("contain").map(_.trim))
  private val names = splitted.map(_(0).split("bags")(0).trim)
  private val containing = splitted.map(_(1).split(',').map(contained => {
    val amountString = contained.trim.split("bags*")(0).trim
    val spaceIndex = amountString.indexOf(' ')
    if (amountString.substring(0, spaceIndex) == "no") (0, "")
    else (amountString.substring(0, spaceIndex).toInt, amountString.substring(spaceIndex+1))
  }))
  private val rules = names.zip(containing).toMap

  private def findContainers(name: String): Set[String] =
    rules.filter(_._2.map(_._2).contains(name)).keys.toSet

  private def findContainersRecursive(name: String): Set[String] = {
    val containers = findContainers(name)
    if (containers.isEmpty) Set.empty[String]
    else containers ++ containers.flatMap(findContainersRecursive)
  }

  def one: Int = findContainersRecursive("shiny gold").size

  private def countContaineesRecursive(name: String): Int = {
    if (rules(name)(0)._1 == 0) 0
    else rules(name).map(c => c._1 + c._1 * countContaineesRecursive(c._2)).sum
  }

  def two: Int = countContaineesRecursive("shiny gold")
}
