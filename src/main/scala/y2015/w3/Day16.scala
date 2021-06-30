package y2015.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day16 extends Day(inputPath(2015, 16)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def processData(data: Array[String]): Map[Int, Map[String, Int]] = data.map(l => {
    val indexAndProps = l.splitAt(l.indexOf(':'))
    val index = indexAndProps._1.split(' ')(1).toInt
    val props = indexAndProps._2.substring(1).split(',').map(p => {
      val parts = p.tail.split(':')
      (parts(0), parts(1).tail.toInt)
    }).toMap
    (index, props)
  }).toMap

  private val allSues = processData(data)
  private val theSue = Map("children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3, "akitas" -> 0,
    "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1)

  def one: Int = allSues.find(s => s._2.forall(si => theSue.contains(si._1) && theSue(si._1) == si._2)).get._1

  def two: Int = allSues.find(s => s._2.forall(si => theSue.contains(si._1) && (
    if (si._1 == "cats" || si._1 == "trees") theSue(si._1) < si._2
    else if (si._1 == "pomeranians" || si._1 == "goldfish") theSue(si._1) > si._2
    else theSue(si._1) == si._2)
  )).get._1
}