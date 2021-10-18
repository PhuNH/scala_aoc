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
    val (min_x, max_x): (Int, Int) = (pos.x - r, pos.x + r)
    val (min_y, max_y): (Int, Int) = (pos.y - r, pos.y + r)
    val (min_z, max_z): (Int, Int) = (pos.z - r, pos.z + r)

    def isInRange(otherPos: Vector): Boolean = otherPos.manhattan(pos) <= r
  }

  private object Nanobot {
    def overlappedRange(indices: IndexedSeq[Int]): Set[Vector] = {
      val min_x = indices.map(nanobots(_).min_x).max
      val max_x = indices.map(nanobots(_).max_x).min
      val min_y = indices.map(nanobots(_).min_y).max
      val max_y = indices.map(nanobots(_).max_y).min
      val min_z = indices.map(nanobots(_).min_z).max
      val max_z = indices.map(nanobots(_).max_z).min
      (for {
        x <- min_x to max_x
        y <- min_y to max_y
        z <- min_z to max_z
        p = Vector(x, y, z)
        if indices.forall(nanobots(_).isInRange(p))
      } yield p).toSet
    }
  }

  private val nanobots: Array[Nanobot] = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(l => {
    val parts = l.split(Array('<', '>'))
    val pos = Vector(parts(1).split(',').map(_.toInt))
    val r = parts(2).split('=')(1).toInt
    Nanobot(pos, r)
  }))

  private def inRangeOf(nanobot: Nanobot): Array[Nanobot] = nanobots.filter(b => nanobot.isInRange(b.pos))

  def one: Int = {
    val strongest = nanobots.maxBy(_.r)
    inRangeOf(strongest).length
  }

  @tailrec
  private def largestCombination(size: Int): Set[Vector] = {
    println(size)
    nanobots.indices.combinations(size).find(c => Nanobot.overlappedRange(c).nonEmpty) match {
      case None => largestCombination(size - 1)
      case Some(c) => Nanobot.overlappedRange(c)
    }
  }

  def two: Int = {
    largestCombination(nanobots.length).minBy(_.manhattan).manhattan
  }
}