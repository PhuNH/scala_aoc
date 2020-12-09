package y2019.w4

import common.Day
import common.Utils._
import y2019._

import scala.collection.mutable.ArrayBuffer

class Day17 extends Day(inputPath(2019, 17)) {
  private val Scaffold = '#'
  private val Up = '^'
  private val Down = 'v'
  private val Left = '<'
  private val Right = '>'

  private val scaffoldPoints = Set(Scaffold, Up, Down, Left, Right).map(_.toInt)

  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private def convertToGrid(outputs: ArrayBuffer[Long]): ExpandableGrid = {
    @scala.annotation.tailrec
    def splitData(data: ArrayBuffer[Int], acc: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
      val splitIndex = data.indexOf('\n')
      if (splitIndex == -1) {
        if (data.nonEmpty) acc += data else acc
      } else {
        val splitted = data.splitAt(splitIndex)
        splitData(splitted._2.tail, if (splitted._1.nonEmpty) acc += splitted._1 else acc)
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

  private def inputRule(program: Intcode, rule: Seq[Int]): Unit = {
    rule.foreach(code => {
      program.setInput(code)
      program.run()
    })
    program.setInput(10)
    program.run()
  }

  private def inputRules(program: Intcode, rules: Seq[Int]*): Unit = {
    rules.foreach(rule => inputRule(program, rule))
  }

  private def inputSelection(program: Intcode, outputs: ArrayBuffer[Long]): Unit = {
    program.setInput('n'.toInt)
    program.run()
    program.setInput(10)
    program.run(outputs)
  }

  def two: Long = {
    val codes = this.codes.clone()
    codes(0) = 2
    val program = Intcode(codes)

    val mainRoutine = "A,B,A,C,A,B,C,A,B,C".map(_.toInt)
    val A = "R,12,R,4,R,10,R,12".map(_.toInt)
    val B = "R,6,L,8,R,10".map(_.toInt)
    val C = "L,8,R,4,R,4,R,6".map(_.toInt)

    inputRules(program, mainRoutine, A, B, C)

    val outputs = ArrayBuffer(0L)
    inputSelection(program, outputs)
    outputs(outputs(0).toInt)
  }
}
