package y2020.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day5 extends Day(inputPath(2020, 5)) {
  private val passes = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def getRow(code: String): Int =
    code.reverse.zipWithIndex.map { case (c, i) => if (c == 'F') 0 else 1 << i }.sum

  private def getCol(code: String): Int =
    code.reverse.zipWithIndex.map { case (c, i) => if (c == 'L') 0 else 1 << i }.sum

  private def getId(code: String): Int =
    (getRow(code.substring(0,7)) << 3) + getCol(code.substring(7))

  private val ids = passes.map(getId)

  def one: Int = ids.max

  def two: Int = {
    val avg = ids.sum / ids.length
    Range.inclusive(ids.min, ids.max).filter(!ids.contains(_)).minBy(id => math.abs(id - avg))
  }
}
