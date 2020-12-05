package y2018.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day5 extends Day(inputPath(2018, 5)) {
  private val polymer = using(Source.fromResource(inputs(0)))(_.getLines().next())

  private def react(polymer: String): String = {
    var res = polymer
    var i = 0
    while (i < res.length-1)
      if (math.abs(res(i) - res(i+1)) != 32) i += 1
      else {
        res = res.substring(0,i) + res.substring(i+2)
        if (i > 0) i -= 1
      }
    res
  }

  def one: Int = react(polymer).length

  def two: Int = {
    val types = polymer.toLowerCase().toSet
    types.map(t => react(polymer.filter(_.toLower != t)).length).min
  }
}
