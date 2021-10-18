package y2018.w4

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day23 extends Day(inputPath(2018, 23)) {
  private case class Vector(x: Int, y: Int, z: Int) {
    def -(that: Vector): Vector = Vector(x - that.x, y - that.y, z - that.z)

    def manhattan: Int = x.abs + y.abs + z.abs

    def manhattan(that: Vector): Int = {
      if (this == Vector.zero) that.manhattan
      else if (that == Vector.zero) this.manhattan
      else (this - that).manhattan
    }
  }

  private object Vector {
    def apply(parts: Array[Int]): Vector = new Vector(parts(0), parts(1), parts(2))

    def zero: Vector = Vector(0, 0, 0)
  }

  private case class Nanobot(pos: Vector, r: Int) {
    def ranges: Set[Vector] = (for {
      i <- -r + pos.x to r + pos.x
      j <- -r + pos.y to r + pos.y
      k <- -r + pos.z to r + pos.z
      if (Vector(i,j,k) - pos).manhattan <= r
    } yield Vector(i, j, k)).toSet
  }

  private val nanobots: Array[Nanobot] = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(l => {
    val parts = l.split(Array('<', '>'))
    val pos = Vector(parts(1).split(',').map(_.toInt))
    val r = parts(2).split('=')(1).toInt
    Nanobot(pos, r)
  }))

  private def inRangeOf(nanobot: Nanobot): Array[Nanobot] = nanobots.filter(_.pos.manhattan(nanobot.pos) <= nanobot.r)

  def one: Int = {
    val strongest = nanobots.maxBy(_.r)
    inRangeOf(strongest).length
  }

  private def region(group: IndexedSeq[Int], nanobotRanges: Array[Set[Vector]],
                     groups: Map[Set[Int], Set[Vector]]): Set[Vector] =
    if (groups.contains(group.toSet)) groups(group.toSet)
    else {
      val ranges = group.map(nanobotRanges(_))
      val result = ranges.reduce(_ & _)
      result
    }

  @tailrec
  private def find(groups: Map[Set[Int], Set[Vector]], step: Int, seenGroups: Map[Set[Int], Set[Vector]],
                   nanobotIndices: Set[Int], nanobotRanges: Array[Set[Vector]]): Int = {
    val newGroups = groups.flatMap { case (groupIndices, groupRegion) =>
      val leftIndices = nanobotIndices &~ groupIndices
      val addedIndexGroups =
        if (step == 1) leftIndices.toArray.map(Array(_))
        else leftIndices.toArray.combinations(step).filter(g => seenGroups.contains(g.toSet)).toArray
      addedIndexGroups
        .map(addedIndices => (groupIndices ++ addedIndices.toSet,
                              groupRegion & region(addedIndices, nanobotRanges, seenGroups)))
        .filter(_._2.nonEmpty).toMap
    }
    if (newGroups.nonEmpty) find(newGroups, step, seenGroups ++ newGroups, nanobotIndices, nanobotRanges)
    else if (step > 1) find(groups, step / 2, seenGroups, nanobotIndices, nanobotRanges)
    else groups.values.flatten.map(_.manhattan).min
  }

  def two: Int = {
    val nanobotRanges = nanobots.map(_.ranges)
    val nanobotIndices = nanobots.indices.toSet
    val groupsWithEmpty = nanobotRanges.indices.combinations(2)
      .map(g => (g.toSet, region(g, nanobotRanges, Map.empty))).toArray
    val groups = groupsWithEmpty.filter(_._2.nonEmpty).toMap
    find(groups, 2, groups, nanobotIndices, nanobotRanges)
  }
}