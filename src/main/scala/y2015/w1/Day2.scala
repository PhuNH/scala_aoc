package y2015.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day2 extends Day(inputPath(2015, 2)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val boxes: Array[Array[Int]] = data.map(_.split('x').map(_.toInt))
  private val measures: Array[Array[Int]] = boxes.map(b => {
    val side_areas = Array(b(0)*b(1), b(1)*b(2), b(2)*b(0))
    val side_perimeters = Array(2*(b(0)+b(1)), 2*(b(1)+b(2)), 2*(b(2)+b(0)))
    Array(side_areas.sum*2 + side_areas.min, side_perimeters.min+b(0)*b(1)*b(2))
  })

  def one: Int = {
    measures.map(_(0)).sum
  }

  def two: Int = {
    measures.map(_(1)).sum
  }
}