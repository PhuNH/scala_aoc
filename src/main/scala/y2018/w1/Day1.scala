package y2018.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day1 extends Day(inputPath(2018, 1)) {
  private val changes: Array[Int] = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(_.trim.toInt))

  def one: Int = changes.sum

  def two: Int = {
    var a: Array[Int] = Array(0)
    var f = a(0)
    do {
      for (i <- changes) {
        f += i
        if (a.contains(f)) return f
        else a = a.:+(f)
      }
      println(f)
    } while (true)
    -1
  }
}
