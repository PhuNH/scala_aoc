package y2015.w4

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day25 extends Day(inputPath(2015, 25)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)(0).split(' ')
  private val row = data(data.length-3).init.toInt
  private val column = data.last.init.toInt

  private def nextPos(rc: (Int, Int)): (Int, Int) = if (rc._1 == 1) (rc._2+1, 1) else (rc._1-1, rc._2+1)

  private def nextNum(prev: Long): Long = prev * 252533 % 33554393

  @tailrec
  private def doUntil(target: (Int, Int), currentPos: (Int, Int), currentNum: Long): Long =
    if (currentPos == target) currentNum
    else doUntil(target, nextPos(currentPos), nextNum(currentNum))

  def one: Long = doUntil((row, column), (6, 6), 27995004)

  def two: Unit = {}
}