package y2015.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day1 extends Day(inputPath(2015, 1)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  def one: Int = {
    data(0).count(_ == '(') - data(0).count(_ == ')')
  }

  def two: Int = {
    data(0).zipWithIndex.find(p => {
      val sub = data(0).substring(0, p._2+1)
      sub.count(_ == '(') == sub.count(_ == ')') - 1
    }).get._2+1
  }
}