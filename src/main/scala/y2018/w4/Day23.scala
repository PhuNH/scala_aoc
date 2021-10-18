package y2018.w4

import common.Day
import common.Utils._

import scala.collection.mutable
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
    def isInRange(otherPos: Vector): Boolean = otherPos.manhattan(pos) <= r
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

  def two: Int = {
    // https://www.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecfkmyo/?context=8&depth=9
    val q = mutable.PriorityQueue.empty[(Int, Int)](Ordering.by[(Int, Int), Int](_._1).reverse)
    nanobots.foreach(b => {
      val d = b.pos.manhattan
      q.enqueue((0.max(d - b.r), 1))
      q.enqueue((d + b.r, -1))
    })
    var (count, maxCount, result) = (0, 0, 0)
    while (q.nonEmpty) {
      val (dist, side) = q.dequeue()
      count += side
      if (count > maxCount) {
        maxCount = count
        result = dist
      }
    }
    result
  }
}