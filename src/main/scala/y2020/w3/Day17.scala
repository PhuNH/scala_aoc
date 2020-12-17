package y2020.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day17 extends Day(inputPath(2020, 17), testPath(2020, 17, 1)) {
  type Point = (Int, Int, Int, Int)
  type Grid = Array[Array[Array[Array[Char]]]]
  private val Active = '#'
  private val Inactive = '.'

  private val coreLayer: Array[Array[Char]] = using(Source.fromResource(inputs(0)))(
    _.getLines().toArray.map(_.toCharArray))
  private val initialGrid = Array.fill(13)(Array.fill(13)(Array.fill(coreLayer.length+12)(Array.fill(coreLayer(0).length+12)(Inactive))))
  for (i <- coreLayer.indices) for (j <- coreLayer(0).indices) initialGrid(6)(6)(6+i)(6+j) = coreLayer(i)(j)

  private def neighbors(p: Point): Array[Point] = (for {
    i <- -1 to 1
    j <- -1 to 1
    k <- -1 to 1
    l <- -1 to 1
    if i != 0 || j != 0 || k != 0 || l != 0
  } yield (p._1+i, p._2+j, p._3+k, p._4+l)).toArray

  private def atCoords(cubes: Grid, p: Point): Char = {
    if (p._4 < 0 || p._4 >= cubes.length || p._3 < 0 || p._3 >= cubes(0).length
      || p._2 < 0 || p._2 >= cubes(0)(0).length || p._1 < 0 || p._1 >= cubes(0)(0)(0).length) Inactive
    else cubes(p._4)(p._3)(p._2)(p._1)
  }

  private def applyRules(prevCubes: Grid, cubes: Grid, p: Point): Unit = {
    val activeNeighbors = neighbors(p).map(atCoords(prevCubes, _)).count(_ == Active)
    if (atCoords(prevCubes, p) == Active && (activeNeighbors != 2 && activeNeighbors != 3)) cubes(p._4)(p._3)(p._2)(p._1) = Inactive
    else if (atCoords(prevCubes, p) == Inactive && activeNeighbors == 3) cubes(p._4)(p._3)(p._2)(p._1) = Active
  }

  private def cloneGrid(from: Grid, to: Grid): Unit =
    for (l <- to.indices) for (k <- to(0).indices) for (i <- to(0)(0).indices) to(l)(k)(i) = from(l)(k)(i).clone()

  def one: Int = {
    val curGrid = Array.fill(13)(Array.fill(13)(new Array[Array[Char]](coreLayer.length+12)))
    cloneGrid(initialGrid, curGrid)
    val prevGrid = Array.fill(13)(Array.fill(13)(new Array[Array[Char]](coreLayer.length+12)))
    cloneGrid(initialGrid, prevGrid)
    for (t <- 1 to 6) {
      for (l <- -t until 1+t) for (k <- -t until 1+t) for (i <- -t until coreLayer.length+t) for (j <- -t until coreLayer(0).length+t)
        applyRules(prevGrid, curGrid, (6+j, 6+i, 6+k, 6+l))
      cloneGrid(curGrid, prevGrid)
    }
    curGrid.map(_.map(_.map(_.count(_ == Active)).sum).sum).sum
  }

  def two: Unit = {}
}