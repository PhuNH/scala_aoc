package y2019.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day8 extends Day(inputPath(2019, 8)) {
  private val Black = '0'
  private val White = '1'
  private val Trans = '2'

  private val encoded: String = using(Source.fromResource(inputs.head))(_.getLines().next())

  private val (width, height) = (25, 6)

  @scala.annotation.tailrec
  private def getSubarrays(array: Array[Char], subarrays: List[Array[Char]], length: Int): List[Array[Char]] =
    array.length match {
      case l if l > length => getSubarrays(array.drop(length), subarrays :+ array.take(length), length)
      case _ => subarrays :+ array
    }

  private val layers: List[Array[Char]] = getSubarrays(encoded.toArray, Nil, width * height)

  def one: Int = {
    val layer = layers.minBy(_.count(_ == '0'))
    layer.count(_ == '1') * layer.count(_ == '2')
  }

  def two: Unit = {
    val decoded = layers.head.zipWithIndex.map(p => {
      val charsAt = layers.map(_.charAt(p._2))
      charsAt.find(_ != '2') match {
        case Some(c) => c
        case _ => '2'
      }
    })
    getSubarrays(decoded, Nil, width).foreach(r => println(r.mkString(" ")))
  }
}