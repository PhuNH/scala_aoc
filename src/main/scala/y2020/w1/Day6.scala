package y2020.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day6 extends Day(inputPath(2020, 6)) {
  private val yesGroups = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
    .reduce(_ + " " + _).split(" {2}").map(_.split(' '))

  def one: Int = {
    yesGroups.map(_.reduce(_ + _).toSet.size).sum
  }

  def two: Int = {
    yesGroups.map(_.fold(Range.inclusive('a','z').map(_.toChar).toString)(_.toSeq.intersect(_).unwrap).length).sum
  }
}
