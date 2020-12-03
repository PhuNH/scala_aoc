package y2020.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day3 extends Day(inputPath(2020, 3)) {
  private val map: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val mapWidth = map.head.length
  private val mapHeight = map.length
  private val start = (0, 0) // (x, y)

  private def moveOnce(cur: (Int, Int), slope: (Int, Int)): (Int, Int) = ((cur._1+slope._1) % mapWidth, cur._2+slope._2)

  @scala.annotation.tailrec
  private def moveFrom(start: (Int, Int), slope: (Int, Int), trees: Int): Int = {
    val newStart = moveOnce(start, slope)
    if (newStart._2 >= mapHeight) trees
    else moveFrom(newStart, slope, if (map(newStart._2)(newStart._1) == '#') trees+1 else trees)
  }

  def one: Int = moveFrom(start, (3, 1), 0)

  def two: Long = {
    moveFrom(start, (1, 1), 0).toLong *
    moveFrom(start, (3, 1), 0) *
    moveFrom(start, (5, 1), 0) *
    moveFrom(start, (7, 1), 0) *
    moveFrom(start, (1, 2), 0)
  }
}
