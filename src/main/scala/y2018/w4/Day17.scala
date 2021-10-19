package y2018.w4

import common.{Coords, Day}
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day17 extends Day(inputPath(2018, 17)) {
  private val source = Coords(0, 500)
  private val regex = """([xy])=(\d+), ([xy])=(\d+)\.\.(\d+)""".r
  private val clays = using(Source.fromResource(inputs(0)))(
    _.getLines().flatMap {
      case regex("x", x, "y", y1, y2) => (y1.toInt to y2.toInt).map(Coords(_, x.toInt))
      case regex("y", y, "x", x1, x2) => (x1.toInt to x2.toInt).map(Coords(y.toInt, _))
    }.toSet)
  private val (smallestY, largestY) = (clays.map(_.v).min, clays.map(_.v).max)

  @tailrec
  private def flow(buffer: Set[Coords], water: Map[Coords, Boolean]): Map[Coords, Boolean] = {
    val springs = buffer.filter(_.v <= largestY)

    if (springs.isEmpty) water
    else {
      val below = springs.head.south
      water.get(below) match {
        case Some(false) => flow(springs.tail, water + (springs.head -> false))
        case None if !clays.contains(below) => flow(springs.tail + below, water + (springs.head -> false))
        case _ =>
          def walk(f: Coords => Coords): Seq[Coords] = {
            lazy val stream: LazyList[Coords] = springs.head #:: stream.map(f)
            stream.takeWhile(p => !clays.contains(p) && (clays.contains(p.south) || water.contains(p.south)))
          }

          val (left, right) = (walk(_.west), walk(_.east))
          val sides = Seq(left.last.west, right.last.east).filter(!clays.contains(_))
          val all = left ++ right ++ sides

          val newSprings = springs.tail.filter(p => !all.contains(p))

          if (sides.nonEmpty) flow(newSprings ++ sides, water ++ all.map(_ -> false))
          else flow(newSprings + springs.head.north, water ++ all.map(_ -> true))
      }
    }
  }

  private val water = flow(Set(source), Map.empty).filter(w => w._1.v >= smallestY && w._1.v <= largestY).values

  def one: Int = water.size

  def two: Unit = {}
}