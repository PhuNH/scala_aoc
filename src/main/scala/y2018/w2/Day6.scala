package y2018.w2

import common.{Coords, Day}
import common.Utils._

import scala.io.Source

class Day6 extends Day(inputPath(2018, 6), testPath(2018, 6, 1)) {
  private val coords = using(Source.fromResource(inputs(0)))(_.getLines().toArray
    .map(_.split(',').map(_.trim.toInt)).map(c => Coords(c(0), c(1))))

  private def indexWithMinDist(loc: Coords): Int = {
    val indicesWithDists = coords.zipWithIndex.map(ci => (ci._2, ci._1.mhtDistWith(loc)))
    val minDist = indicesWithDists.minBy(_._2)._2
    val indicesWithMinDist = indicesWithDists.filter(_._2 == minDist)
    if (indicesWithMinDist.length != 1) -1
    else indicesWithMinDist(0)._1
  }

  private val xMin = coords.map(_.h).min
  private val xMax = coords.map(_.h).max
  private val yMin = coords.map(_.v).min
  private val yMax = coords.map(_.v).max

  private val infiniteIndices: Set[Int] = {
    val borderLocs = Range.inclusive(xMin, xMax).flatMap(x => Array(Coords(x, yMin), Coords(x, yMax))) ++
      Range(yMin+1, yMax).flatMap(y => Array(Coords(xMin, y), Coords(xMax, y)))
    borderLocs.map(indexWithMinDist).filterNot(_ == -1).toSet
  }

  private val finiteIndices = coords.indices.filterNot(infiniteIndices.contains)

  private val land = Array.fill(yMax-yMin+1)(Array.fill(xMax-xMin+1)(-1))

  def one: Int = {
    val finiteOccurrences = (for {
      i <- yMin+1 until yMax
      j <- xMin+1 until xMax
      index = indexWithMinDist(Coords(j, i))
      if index != -1
    } yield index).filterNot(infiniteIndices.contains)
    finiteOccurrences.toSet.map((o: Int) => finiteOccurrences.count(_ == o)).max
  }

  private def distancesToAll(loc: Coords): Int = coords.map(_.mhtDistWith(loc)).sum

  def two: Int = (for {
    i <- yMin to yMax
    j <- xMin to xMax
    d = distancesToAll(Coords(j, i))
    if d < 10000
  } yield 1).sum
}
