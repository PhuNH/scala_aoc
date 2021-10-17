package y2018.w5

import common.Day
import common.Utils._

import scala.collection.mutable
import scala.io.Source

class Day25 extends Day(inputPath(2018, 25)) {
  private case class Vector(x: Int, y: Int, z: Int, t: Int) {
    def -(that: Vector): Vector = Vector(x - that.x, y - that.y, z - that.z, t - that.t)

    def manhattan: Int = x.abs + y.abs + z.abs + t.abs

    def manhattan(that: Vector): Int = {
      if (this == Vector.zero) that.manhattan
      else if (that == Vector.zero) this.manhattan
      else (this - that).manhattan
    }
  }

  private object Vector {
    def apply(parts: Array[Int]): Vector = new Vector(parts(0), parts(1), parts(2), parts(3))

    def zero: Vector = Vector(0, 0, 0, 0)
  }

  private val points: Array[Vector] = using(Source.fromResource(inputs(0)))(
    _.getLines().toArray.map(l => Vector(l.split(',').map(_.toInt))))

  def one: Int = {
    var constellations = Array.empty[mutable.Set[Vector]]
    points.foreach(p => {
      val chained = constellations.zipWithIndex.filter(c => c._1.exists(_.manhattan(p) <= 3)).map(_._2).toSet
      if (chained.isEmpty) constellations = constellations :+ mutable.Set(p)
      else {
        val allChained = mutable.Set.from(chained.flatMap(constellations) + p)
        constellations = constellations.zipWithIndex.filterNot(ci => chained.contains(ci._2)).map(_._1)
        constellations = constellations :+ allChained
      }
    })
    constellations.length
  }

  def two: Unit = {}
}