package y2015.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day15 extends Day(inputPath(2015, 15)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def processData(data: Array[String]): Array[Array[Int]] =
    data.map(l => {
      val parts = l.split(' ')
      Array(parts(2).init.toInt, parts(4).init.toInt, parts(6).init.toInt, parts(8).init.toInt, parts(10).toInt)
    })

  private val ingredients = processData(data)

  def one: Int = (for {
    iF1 <- 1 to 97
    iF2 <- 1 to (98-iF1)
    iF3 <- 1 to (99-iF1-iF2)
    iF4 = 100-iF1-iF2-iF3
  } yield {
    val factors = Array(iF1, iF2, iF3, iF4)
    val props = (0 to 3).map(prop => (0 to 3).map(iF => factors(iF) * ingredients(iF)(prop)).sum)
    if (props.exists(_ <= 0)) 0 else props.product
  }).max

  def two: Int = (for {
    iF1 <- 1 to 97
    iF2 <- 1 to (98-iF1)
    iF3 <- 1 to (99-iF1-iF2)
    iF4 = 100-iF1-iF2-iF3
  } yield {
    val factors = Array(iF1, iF2, iF3, iF4)
    val props = (0 to 4).map(prop => (0 to 3).map(iF => factors(iF) * ingredients(iF)(prop)).sum)
    if (props(4) != 500 || props.slice(0,4).exists(_ <= 0)) 0 else props.slice(0,4).product
  }).max
}