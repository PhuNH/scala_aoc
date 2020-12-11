package y2020.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day11 extends Day(inputPath(2020, 11), testPath(2020, 11, 1)) {
  type Layout = Array[Array[Char]]
  private val FLOOR = '.'
  private val EMPTY = 'L'
  private val OCCUP = '#'

  private val layout: Layout = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(_.toCharArray))
  private val width = layout(0).length
  private val length = layout.length

  private def adjacentSeats(x: Int, y: Int, baseLayout: Layout): IndexedSeq[Char] = for {
    i <- (if (y == 0) y else y-1) to (if (y == length-1) y else y+1)
    j <- (if (x == 0) x else x-1) to (if (x == width-1) x else x+1)
    if i != y || j != x
  } yield baseLayout(i)(j)

  private def directionalSeats(x: Int, y: Int, baseLayout: Layout): IndexedSeq[Char] = {
    //import Numeric.Implicits._
    def seatInDirection(xDir: Int, yDir: Int): Char = {
      (xDir, yDir) match {
        case (-1, -1) =>
          for (i <- 1 to x.min(y)) if (baseLayout(y-i)(x-i) != FLOOR) return baseLayout(y-i)(x-i)
        case (-1, 0) =>
          for (i <- 1 to x) if (baseLayout(y)(x-i) != FLOOR) return baseLayout(y)(x-i)
        case (-1, 1) =>
          for (i <- 1 to x.min(length-y-1)) if (baseLayout(y+i)(x-i) != FLOOR) return baseLayout(y+i)(x-i)
        case (0, 1) =>
          for (i <- 1 until length-y) if (baseLayout(y+i)(x) != FLOOR) return baseLayout(y+i)(x)
        case (1, 1) =>
          for (i <- 1 to (width-x-1).min(length-y-1)) if (baseLayout(y+i)(x+i) != FLOOR) return baseLayout(y+i)(x+i)
        case (1, 0) =>
          for (i <- 1 until width-x) if (baseLayout(y)(x+i) != FLOOR) return baseLayout(y)(x+i)
        case (1, -1) =>
          for (i <- 1 to (width-x-1).min(y)) if (baseLayout(y-i)(x+i) != FLOOR) return baseLayout(y-i)(x+i)
        case (0, -1) =>
          for (i <- 1 to y) if (baseLayout(y-i)(x) != FLOOR) return baseLayout(y-i)(x)
      }
      FLOOR
    }

    for {
      yDir <- -1 to 1
      xDir <- -1 to 1
      if yDir != 0 || xDir != 0
    } yield seatInDirection(xDir, yDir)
  }

  private def seatsToCheck(seatType: Int): (Int, Int, Layout) => IndexedSeq[Char] =
    if (seatType == 1) adjacentSeats else directionalSeats

  private def checkRules(x: Int, y: Int, base: Layout, current: Layout, seatType: Int): Boolean =
    if (base(y)(x) == EMPTY && !seatsToCheck(seatType)(x, y, base).contains(OCCUP)) {
      current(y)(x) = OCCUP
      true
    } else if (base(y)(x) == OCCUP && seatsToCheck(seatType)(x, y, base).count(_ == OCCUP) >= 3+seatType) {
      current(y)(x) = EMPTY
      true
    } else false

  private def checkRulesAll(base: Layout, current: Layout, seatType: Int): Boolean = (for {
    y <- base.indices
    x <- base(0).indices
  } yield checkRules(x, y, base, current, seatType)).forall(_ == false)

  private def cloneLayout(from: Layout, to: Layout): Unit = for (i <- to.indices) to(i) = from(i).clone()

  private def getSeats(seatType: Int): Int = {
    val base = new Layout(length)
    val temp = new Layout(length)
    cloneLayout(layout, base)
    cloneLayout(base, temp)
    var i = 0
    while (!checkRulesAll(base, temp, seatType)) {
      cloneLayout(temp, base)
      i += 1
    }
    println(i)
    base.map(_.count(_ == OCCUP)).sum
  }

  def one: Int = getSeats(1)

  def two: Int = getSeats(2)
}