package y2020.w3

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day17 extends Day(inputPath(2020, 17), testPath(2020, 17, 1)) {
  case class Cube(x: Int, y: Int, z: Int = 0, t: Int = 0)

  private val space: Set[Cube] = using(Source.fromResource(inputs(0)))(
    _.getLines().toIndexedSeq.zipWithIndex.flatMap(e => e._1.zipWithIndex.flatMap {
      case ('#', x) => Some(Cube(x, e._2))
      case _ => None
    })).toSet

  private def neighbors(c: Cube, fourD: Boolean): IndexedSeq[Cube] = {
    val r = -1 to 1
    for { i <- r; j <- r; k <- r; l <- if (fourD) r else Seq(0); if Seq(i, j, k, l).exists(_ != 0) }
      yield Cube(c.x+i, c.y+j, c.z+k, c.t+l)
  }

  private def applyRules(c: Cube, space: Set[Cube], fourD: Boolean): Boolean = {
    val activeNeighbors = neighbors(c, fourD).count(space.contains)
    if (space.contains(c)) activeNeighbors == 2 || activeNeighbors == 3
    else activeNeighbors == 3
  }

  @tailrec
  private def simulate(space: Set[Cube], steps: Int, fourD: Boolean = false): Set[Cube] = {
    val neighborSpace = space.flatMap(neighbors(_, fourD))
    val nextSpace = neighborSpace.filter(applyRules(_, space, fourD))
    if (steps == 1) nextSpace
    else simulate(nextSpace, steps-1, fourD)
  }

  def one: Int = simulate(space, 6).size

  def two: Int = simulate(space, 6, fourD = true).size
}