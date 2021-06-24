package y2015.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day6 extends Day(inputPath(2015, 6)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def coordsToRanges(lls: String, urs: String): (Range, Range) = {
    val ll = lls.split(',').map(_.toInt)
    val ur = urs.split(',').map(_.toInt)
    (Range.inclusive(ll(0), ur(0)), Range.inclusive(ll(1), ur(1)))
  }

  private def parseInstruction(words: Array[String]): ((Range, Range), Int) =
    if (words.length == 4) (coordsToRanges(words(1), words(3)), 2)
    else (coordsToRanges(words(2), words(4)), if (words(1).length == 2) 1 else 0)

  private def processInstruction(ins: String, grid: Array[Array[Int]])(processFunction: (Int, Int) => Int): Unit = {
    val words = ins.split(' ')
    val (ranges, action) = parseInstruction(words)
    for (i <- ranges._2) for (j <- ranges._1) {
      grid(i)(j) = processFunction(action, grid(i)(j))
    }
  }

  def one: Int = {
    val grid: Array[Array[Int]] = Array.fill(1000)(Array.fill(1000)(0))
    def processFunction(action: Int, oldVal: Int): Int = action match {
      case 2 => 1-oldVal
      case x => x
    }
    data.foreach(processInstruction(_, grid)(processFunction))
    grid.map(_.count(l => l == 1)).sum
  }

  def two: Int = {
    val grid: Array[Array[Int]] = Array.fill(1000)(Array.fill(1000)(0))
    def processFunction(action: Int, oldVal: Int): Int = action match {
      case 0 => if (oldVal > 0) oldVal-1 else oldVal
      case x => oldVal+x
    }
    data.foreach(processInstruction(_, grid)(processFunction))
    grid.map(_.sum).sum
  }
}