package y2015.w3

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day18 extends Day(inputPath(2015, 18)) {
  case class Cell(x: Int, y: Int)

  private val grid: Set[Cell] = using(Source.fromResource(inputs(0)))(
    _.getLines().toIndexedSeq.zipWithIndex.flatMap(l => l._1.zipWithIndex.flatMap {
      case ('#', x) => Some(Cell(x, l._2))
      case _ => None
    })).toSet

  private def neighbors(c: Cell): IndexedSeq[Cell] = {
    val r = -1 to 1
    val l = 0 until 100
    for {
      i <- r; j <- r;
      if Seq(i, j).exists(_ != 0) && l.contains(c.x+i) && l.contains(c.y+j)
    } yield Cell(c.x+i, c.y+j)
  }

  private def applyRules(c: Cell, grid: Set[Cell]): Boolean = {
    val activeNeighbors = neighbors(c).count(grid.contains)
    if (grid.contains(c)) activeNeighbors == 2 || activeNeighbors == 3
    else activeNeighbors == 3
  }

  @tailrec
  private def simulate(grid: Set[Cell], steps: Int): Set[Cell] = {
    val neighborSpace = grid.flatMap(neighbors)
    val nextGrid = neighborSpace.filter(applyRules(_, grid))
    if (steps == 1) nextGrid
    else simulate(nextGrid, steps-1)
  }

  def one: Int = simulate(grid, 100).size

  private val stuck = Set(Cell(0, 0), Cell(0, 99), Cell(99, 0), Cell(99, 99))
  @tailrec
  private def simulateStuck(grid: Set[Cell], steps: Int): Set[Cell] = {
    val neighborSpace = grid.flatMap(neighbors)
    val nextGrid = neighborSpace.filter(applyRules(_, grid)) ++ stuck
    if (steps == 1) nextGrid
    else simulateStuck(nextGrid, steps-1)
  }

  def two: Int = simulateStuck(grid ++ stuck, 100).size
}