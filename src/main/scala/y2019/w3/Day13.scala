package y2019.w3

import common.Day
import common.Utils._
import y2019.Intcode

import scala.collection.mutable.ArrayBuffer
import scala.math.signum

class Day13 extends Day(inputPath(2019, 13)) {
  type Grid = ArrayBuffer[ArrayBuffer[Int]]
  type Coord = (Int, Int)

  private val Empty = 0
  private val Wall = 1
  private val Block = 2
  private val Paddle = 3
  private val Ball = 4

  private val InitialGridSize = 9
  private val Down = 0
  private val Right = 1
  private val Left = -1
  private val Neutral = 0

  private def draw(outputs: ArrayBuffer[Int], grid: Grid, score: Array[Int]): Unit = {
    var i = 1
    var gridWidth = grid(0).length
    var gridHeight = grid.length

    def adjust(direction: Int, length: Int): Unit = direction match {
      case Right =>
        grid.map(_.appendAll(new Array[Int](length - gridWidth)))
        gridWidth = length
      case Down =>
        val toBeAppended = ArrayBuffer.fill(length - gridHeight)(ArrayBuffer.fill(gridWidth)(Empty))
        grid.appendAll(toBeAppended)
        gridHeight = length
    }

    while (i <= outputs(0)) {
      if (outputs(i) == -1 && outputs(i+1) == 0)
        score(0) = outputs(i+2)
      else {
        if (outputs(i+1) >= gridHeight) adjust(Down, outputs(i+1)+1)
        if (outputs(i) >= gridWidth) adjust(Right, outputs(i)+1)
        grid(outputs(i+1))(outputs(i)) = outputs(i+2)
      }
      i += 3
    }
  }

  private def findInGrid(grid: Grid, what: Int): Coord = {
    grid.map(_.indexOf(what)).zipWithIndex.filter(_._1 != -1) match {
      case ArrayBuffer() => (-1, -1)
      case l => l.head
    }
  }

  private def computeMove(grid: Grid): Int = {
    val paddlePos = findInGrid(grid, Paddle)
    val ballPos = findInGrid(grid, Ball)

    if (ballPos == (-1, -1)) Neutral
    else signum(ballPos._1 - paddlePos._1)
  }

  private def play(program: Intcode, grid: Grid, score: Array[Int] = Array(0)): Unit = {
    var input = Neutral

    while (program.getState != Intcode.Stopped) {
      program.setInput(input)
      val outputs = ArrayBuffer(0L)
      program.run(outputs)
      draw(outputs.map(_.toInt), grid, score)
      input = computeMove(grid)
    }
  }

  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  def one: Int = {
    val program = Intcode(codes)
    val grid = ArrayBuffer.fill(InitialGridSize)(ArrayBuffer.fill(InitialGridSize)(Empty))
    play(program, grid)
    grid.map(_.count(_ == Block)).sum
  }

  def two: Int = {
    val codes = this.codes.clone()
    codes(0) = 2
    val program = Intcode(codes)
    val grid = ArrayBuffer.fill(InitialGridSize)(ArrayBuffer.fill(InitialGridSize)(Empty))
    val score = Array(0)
    play(program, grid, score)
    score(0)
  }
}