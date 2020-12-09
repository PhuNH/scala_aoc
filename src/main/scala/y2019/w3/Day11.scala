package y2019.w3

import common.Day
import common.Utils._
import y2019.Intcode

import scala.collection.mutable.ArrayBuffer

class Day11 extends Day(inputPath(2019, 11)) {
  private val Black = 0
  private val White = 1
  private val TurnLeft = 0
  private val TurnRight = 1

  private val Up = 0
  private val Right = 1
  private val Down = 2
  private val Left = 3

  private val InitialGridSize = 9
  private val InitialIndex = 4
  private val AdjustSize = 5

  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private def runRobot(program: Intcode, initialPanelColor: Int = Black,
                       grid: ArrayBuffer[ArrayBuffer[Int]], painted: ArrayBuffer[(Int, Int)]): Unit = {
    var gridWidth = grid.size
    var gridHeight = grid(0).size
    var currentDirection = Up
    var currentX = InitialIndex
    var currentY = InitialIndex
    grid(currentY)(currentX) = initialPanelColor

    def adjust(to: Int) = to match {
      case Up =>
        gridHeight += AdjustSize
        val toBePrepended = ArrayBuffer.fill(AdjustSize)(ArrayBuffer.fill(gridWidth)(0))
        grid.prependAll(toBePrepended)
        painted.indices foreach (i => painted.update(i, (painted(i)._1, painted(i)._2+AdjustSize)))
        currentY += AdjustSize
      case Down =>
        gridHeight += AdjustSize
        val toBeAppended = ArrayBuffer.fill(AdjustSize)(ArrayBuffer.fill(gridWidth)(0))
        grid.appendAll(toBeAppended)
      case Left =>
        gridWidth += AdjustSize
        grid.foreach(_.prependAll(new Array[Int](AdjustSize)))
        painted.indices foreach (i => painted.update(i, (painted(i)._1+AdjustSize, painted(i)._2)))
        currentX += AdjustSize
      case Right =>
        gridWidth += AdjustSize
        grid.map(_.appendAll(new Array[Int](AdjustSize)))
    }

    while (program.getState != Intcode.Stopped) {
      program.setInput(grid(currentY)(currentX))
      val outputs = ArrayBuffer(0L)
      program.run(outputs)
      // paint
      grid(currentY)(currentX) = outputs(outputs(0).toInt - 1).toInt
      painted += ((currentX, currentY))
      // turn and move
      outputs(outputs(0).toInt) match {
        case TurnRight =>
          currentDirection += 1
          if (currentDirection > Left) currentDirection = Up
        case TurnLeft =>
          currentDirection -= 1
          if (currentDirection < Up) currentDirection = Left
      }
      currentDirection match {
        case Up => currentY -= 1
        case Right => currentX += 1
        case Down => currentY += 1
        case Left => currentX -= 1
      }
      if (currentY < 0) adjust(Up)
      else if (currentY >= gridHeight) adjust(Down)
      if (currentX < 0) adjust(Left)
      else if (currentX >= gridWidth) adjust(Right)
    }
  }

  def one: Int = {
    val program = Intcode(codes)
    val grid = ArrayBuffer.fill(InitialGridSize)(ArrayBuffer.fill(InitialGridSize)(0))
    val painted = ArrayBuffer[(Int, Int)]()
    runRobot(program, Black, grid, painted)
    painted.toSet.size
  }

  def two: Unit = {
    val program = Intcode(codes)
    val grid = ArrayBuffer.fill(InitialGridSize)(ArrayBuffer.fill(InitialGridSize)(0))
    val painted = ArrayBuffer[(Int, Int)]()
    runRobot(program, White, grid, painted)
    grid.foreach(l => {
      val s = new StringBuilder()
      l.foreach(c => s ++= (if (c == White) "1 " else "  "))
      println(s)
    })
  }
}