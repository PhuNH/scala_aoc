package y2019.w4

import common.Day
import common.Utils._
import y2019._

import scala.collection.mutable.ArrayBuffer

class Day17 extends Day(inputPath(2019, 17)) {
  private val Scaffold = '#'
  private val Space = '.'

  private val Up = '^'
  private val Down = 'v'
  private val Left = '<'
  private val Right = '>'
  private val Tumbling = 'X'

  private val scaffoldPoints = Set(Scaffold, Up, Down, Left, Right).map(_.toInt)
  private val robotPoints = Set(Up, Down, Left, Right, Tumbling).map(_.toInt)

  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private def convertToGrid(outputs: ArrayBuffer[Long]): ExpandableGrid = {
    @scala.annotation.tailrec
    def splitData(data: ArrayBuffer[Int], acc: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
      val splitIndex = data.indexOf('\n')
      if (splitIndex == -1) {
        if (data.nonEmpty) acc.addOne(data) else acc
      } else {
        val splitted = data.splitAt(splitIndex)
        splitData(splitted._2.tail, if (splitted._1.nonEmpty) acc.addOne(splitted._1) else acc)
      }
    }

    val data = splitData(outputs.map(_.toInt), ArrayBuffer.empty[ArrayBuffer[Int]])
    val grid = ExpandableGrid()
    grid.setData(data)
    grid
  }

  private def alignParam(pos: TwoD): Int = pos.v * pos.h

  private def contains(set: Set[Int], view: ExpandableGrid, pts: TwoD*): Boolean =
    pts.forall(p => set.contains(view.getAtPos(p)))

  private def isIntersection(pos: TwoD, view: ExpandableGrid): Boolean = {
    pos.v > 0 && pos.v < view.height-1 && pos.h > 0 && pos.h < view.width-1 &&
      contains(scaffoldPoints, view, pos, pos.north, pos.south, pos.west, pos.east)
  }

  private def findIntersections(view: ExpandableGrid): Array[TwoD] =
    (for {
      i <- 0 until view.height
      j <- 0 until view.width
      p = TwoD(i,j)
      if isIntersection(p, view)
    } yield p).toArray

  def one: Int = {
    val program = Intcode(codes)
    val outputs = ArrayBuffer(0L)
    program.run(outputs)
    val view = convertToGrid(outputs.tail)
    findIntersections(view).map(alignParam).sum
  }

  def two: Unit = {
//    val codes = this.codes.clone()
//    codes(0) = 2
//    val program = Intcode(codes)
  }
}
