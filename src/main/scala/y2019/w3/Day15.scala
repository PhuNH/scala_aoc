package y2019.w3

import common.{Coords, Day, Direction, ExpandableGrid}
import common.Utils._
import y2019.Intcode

import scala.collection.mutable.ArrayBuffer

class Day15 extends Day(inputPath(2019, 15)) {
  private val Unchanged = 0
  private val Moved = 1
  private val Found = 2
  private val Oxidized = 9

  private val OxySearch = 1
  private val MapDraw = 2

  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private def runRobot(program: Intcode, grid: ExpandableGrid[Int],
                       option: Int, oxyPos: Coords = Coords.unknown): Int = {
    var direction = Direction.North
    val originPos = Coords(ExpandableGrid.InitialIndex, ExpandableGrid.InitialIndex)
    val currentPos = originPos.copy()
    val nextPos = Coords.unknown
    var count = 0
    grid.setAtPos(currentPos, Moved)
    val stepStack = scala.collection.mutable.Stack(
      (currentPos.north, Direction.North, count+1), (currentPos.east, Direction.East, count+1),
      (currentPos.south, Direction.South, count+1), (currentPos.west, Direction.West, count+1))

    def adjust(): Unit = direction match {
      case Direction.North if currentPos.v-1 < 0 =>
        grid.expand(Direction.North)
        stepStack.indices foreach(i => stepStack(i)._1.increase(dV = ExpandableGrid.ExpandingSize))
        currentPos.increase(dV = ExpandableGrid.ExpandingSize)
        originPos.increase(dV = ExpandableGrid.ExpandingSize)
        if (oxyPos != Coords.unknown) oxyPos.increase(dV = ExpandableGrid.ExpandingSize)
      case Direction.South if currentPos.v+1 >= grid.length =>
        grid.expand(Direction.South)
      case Direction.West if currentPos.h-1 < 0 =>
        grid.expand(Direction.West)
        stepStack.indices foreach(i => stepStack(i)._1.increase(dH = ExpandableGrid.ExpandingSize))
        currentPos.increase(dH = ExpandableGrid.ExpandingSize)
        originPos.increase(dH = ExpandableGrid.ExpandingSize)
        if (oxyPos != Coords.unknown) oxyPos.increase(dH = ExpandableGrid.ExpandingSize)
      case Direction.East if currentPos.h+1 >= grid.width =>
        grid.expand(Direction.East)
      case _ => ()
    }

    def checkPos(pos: Coords): Boolean =
      grid.getAtPos(pos) != Unchanged && grid.getAtPos(pos) != Moved && !stepStack.exists(_._1 == pos)

    def popStep(): Unit = {
      val popped = stepStack.pop()
      nextPos.updateWith(popped._1)
      direction = popped._2
      count = popped._3
    }

    @scala.annotation.tailrec
    def loop(): Int = {
      if (stepStack.isEmpty) count
      else {
        popStep()
        program.setInput(direction.id)
        val outputs = ArrayBuffer(0L)
        program.run(outputs)
        outputs(outputs(0).toInt) match {
          case Unchanged =>
            grid.setAtPos(nextPos, Unchanged)
            loop()
          case something if something == Moved || (something == Found && option == MapDraw) =>
            currentPos.updateWith(nextPos)
            adjust()

            val eastPos = currentPos.east
            val westPos = currentPos.west
            val northPos = currentPos.north
            val southPos = currentPos.south
            if (grid.getAtPos(currentPos) != Moved)
              direction match {
                case Direction.North => stepStack.push((southPos, Direction.South, count - 1))
                case Direction.South => stepStack.push((northPos, Direction.North, count - 1))
                case Direction.West => stepStack.push((eastPos, Direction.East, count - 1))
                case Direction.East => stepStack.push((westPos, Direction.West, count - 1))
              }
            if (direction != Direction.West && checkPos(eastPos)) stepStack.push((eastPos, Direction.East, count + 1))
            if (direction != Direction.East && checkPos(westPos)) stepStack.push((westPos, Direction.West, count + 1))
            if (direction != Direction.South && checkPos(northPos)) stepStack.push((northPos, Direction.North, count + 1))
            if (direction != Direction.North && checkPos(southPos)) stepStack.push((southPos, Direction.South, count + 1))

            if (something == Found)
              oxyPos.updateWith(currentPos)
            grid.setAtPos(currentPos, Moved)
            loop()
          case _ => // Found and OxySearch
            count
        }
      }
    }
    loop()
  }

  def one: Int = {
    val program = Intcode(codes)
    val grid = ExpandableGrid[Int]()
    runRobot(program, grid, OxySearch)
  }

  private def disperseOxy(grid: ExpandableGrid[Int], oxyPos: Coords): Int = {
    def adjacentPositions(pos: Coords): Seq[Coords] =
      Seq(pos.north, pos.south, pos.west, pos.east).filter(grid.getAtPos(_) == Moved)

    @scala.annotation.tailrec
    def loop(input: Seq[(Coords, Int)]): Int = {
      val count = input.head._2
      input.foreach(x => grid.setAtPos(x._1, Oxidized))
      val output = input.flatMap(x => adjacentPositions(x._1).map((_, count + 1)))
      if (output.isEmpty) count
      else loop(output)
    }

    loop(Seq((oxyPos, 0)))
  }

  def two: Int = {
    val program = Intcode(codes)
    val grid = ExpandableGrid[Int]()
    val oxyPos = Coords.unknown
    runRobot(program, grid, MapDraw, oxyPos)
    disperseOxy(grid, oxyPos)
  }
}