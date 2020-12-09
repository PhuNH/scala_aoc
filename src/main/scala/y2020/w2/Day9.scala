package y2020.w2

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day9 extends Day(inputPath(2020, 9)) {
  private val data: Array[Long] = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(_.toLong))

  private def isValid(theIndex: Int): Boolean = {
    val theseIndices = Range(theIndex-25, theIndex).toSet
    val those = Range(theIndex-25, theIndex).map(data(theIndex) - data(_))
    theseIndices.exists(i => (theseIndices - i).map(data).contains(those(i+25-theIndex)))
  }

  def one: Long = {
    var i = 25
    while (isValid(i)) i += 1
    data(i)
  }

  @tailrec
  private def fromHereBackWith(here: Int, num: Long): Int = {
    if (num < 0) -1
    else if (num == 0) here
    else fromHereBackWith(here-1, num-data(here-1))
  }

  def two: Long = {
    var i = 25
    while (isValid(i)) i += 1
    val num = data(i)
    var start = -1
    do {
      start = fromHereBackWith(i, num)
      i -= 1
    } while (start < 0)
    val range = data.slice(start, i+1)
    range.min + range.max
  }
}