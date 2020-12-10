package y2020.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day10 extends Day(inputPath(2020, 10)) {
  private val data: Array[Int] = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(_.toInt).sorted)
  private val jolts = 0 +: data :+ (data.last+3)
  private val diffs = jolts.indices.tail.map(i => jolts(i) - jolts(i - 1))
  println(jolts.map(_.toString).reduce(_ + " " + _))
  println(diffs.map(_.toString).reduce(_ + " " + _))

  def one: Int = {
    diffs.count(_ == 1) * diffs.count(_ == 3)
  }

  private def waysForLength(l: Int): Long = l match {
    case x if x < 2 => 1
    case 2 => 2
    case 3 => 4
    case 4 => 7
  }

  def two: Long = {
    diffs.map(_.toString).reduce(_ + _).split('3').map(ones => waysForLength(ones.length)).product
  }
}