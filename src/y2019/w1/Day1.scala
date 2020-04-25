package y2019.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day1 extends Day(inputPath(2019, 1)) {
  private val moduleMasses: List[String] = using(Source.fromFile(inputs.head))(_.getLines().toList)

  private def fuelRequired(mass: Int): Int = mass / 3 - 2

  private def totalFuelRequired(mass: Int): Int = {
    @scala.annotation.tailrec
    def require(mass: Int, aggTotal: Int): Int = fuelRequired(mass) match {
      case m if m > 0 => require(m, aggTotal+m)
      case _ => aggTotal
    }
    require(mass, 0)
  }

  def one: Int = moduleMasses.map(l => fuelRequired(l.trim.toInt)).sum

  def two: Int = moduleMasses.map(l => totalFuelRequired(l.trim.toInt)).sum
}
