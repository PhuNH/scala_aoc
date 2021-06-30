package y2015.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day17 extends Day(inputPath(2015, 17)) {
  private val containers: List[Int] = using(Source.fromResource(inputs(0)))(_.getLines().map(_.toInt).toList.sorted)
  private val eggnog = 150

  private def countRec(containers: List[Int], left: Int, used: List[Int]): Array[List[Int]] =
    if (left == 0) Array(used)
    else if (containers.isEmpty || left < 0) Array.empty[List[Int]]
    else countRec(containers.tail, left-containers.head, containers.head :: used) ++
      countRec(containers.tail, left, used)

  private val combinations = countRec(containers, eggnog, List.empty[Int])

  def one: Int = combinations.length

  def two: Int = {
    val minLen = combinations.minBy(_.size).size
    combinations.count(_.size == minLen)
  }
}