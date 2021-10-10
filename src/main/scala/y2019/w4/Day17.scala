package y2019.w4

import common.{Coords, Day, ExpandableGrid}
import common.Utils._
import y2019.{AsciiInterface, Intcode}

import scala.collection.mutable.ArrayBuffer

class Day17 extends Day(inputPath(2019, 17)) {
  private val Scaffold = '#'
  private val Up = '^'
  private val Down = 'v'
  private val Left = '<'
  private val Right = '>'

  private val scaffoldPoints = Set(Scaffold, Up, Down, Left, Right).map(_.toInt)

  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private def alignParam(pos: Coords): Int = pos.v * pos.h

  private def contains(set: Set[Int], view: ExpandableGrid[Int], pts: Coords*): Boolean =
    pts.forall(p => set.contains(view.getAtPos(p)))

  private def isIntersection(pos: Coords, view: ExpandableGrid[Int]): Boolean = {
    pos.v > 0 && pos.v < view.length-1 && pos.h > 0 && pos.h < view.width-1 &&
      contains(scaffoldPoints, view, pos, pos.north, pos.south, pos.west, pos.east)
  }

  private def findIntersections(view: ExpandableGrid[Int]): Array[Coords] =
    (for {
      i <- 0 until view.length
      j <- 0 until view.width
      p = Coords(i,j)
      if isIntersection(p, view)
    } yield p).toArray

  def one: Int = {
    val program = Intcode(codes)
    val outputs = ArrayBuffer(0L)
    program.run(outputs)
    val view = AsciiInterface.ascii2Grid(outputs.tail)
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
