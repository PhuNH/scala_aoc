package y2020.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day2 extends Day(inputPath(2020, 2)) {
  private val polPws: List[Array[String]] = using(Source.fromResource(inputs(0)))(_.getLines().toList.map(_.split(':').map(_.trim)))

  private def isValid(pol: String, pw: String): Boolean = {
    val rangeChar = pol.split(' ')
    val range = rangeChar(0).split('-').map(_.toInt)
    val char = rangeChar(1)(0)
    Range.inclusive(range(0),range(1)).contains(pw.count(_ == char))
  }

  def one: Int = polPws.count(polPw => isValid(polPw(0), polPw(1)))

  private def isValidTwo(pol: String, pw: String): Boolean = {
    val posChar = pol.split(' ')
    val pos = posChar(0).split('-').map(_.toInt)
    val char = posChar(1)(0)
    pos.count(po => po <= pw.length && char == pw(po-1)) == 1
  }

  def two: Int = polPws.count(polPw => isValidTwo(polPw(0), polPw(1)))
}
