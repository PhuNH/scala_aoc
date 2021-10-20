package y2018.w4

import common.{Coords, Day}
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day17 extends Day(inputPath(2018, 17)) {
  private val source = Coords(0, 500)
  private val regex = """([xy])=(\d+), ([xy])=(\d+)\.\.(\d+)""".r
  private val clay = using(Source.fromResource(inputs(0)))(
    _.getLines().flatMap {
      case regex("x", x, "y", y1, y2) => (y1.toInt to y2.toInt).map(Coords(_, x.toInt))
      case regex("y", y, "x", x1, x2) => (x1.toInt to x2.toInt).map(Coords(y.toInt, _))
    }.toSet)
  private val (smallestY, largestY) = (clay.map(_.v).min, clay.map(_.v).max)

  @tailrec
  private def flow(buffer: Set[Coords], water: Map[Coords, Boolean]): Map[Coords, Boolean] = {
    val springs = buffer.filter(_.v <= largestY)

    if (springs.isEmpty) water
    else {
      val below = springs.head.south
      water.get(below) match {
        case Some(false) =>
          flow(springs.tail, water + (springs.head -> false))
        case None if !clay.contains(below) =>
          flow(springs.tail + below, water + (springs.head -> false))
        case _ =>
          def walk(f: Coords => Coords): Seq[Coords] = {
            lazy val stream: LazyList[Coords] = springs.head #:: stream.map(f)
            stream.takeWhile(p => !clay.contains(p) && (clay.contains(p.south) || water.get(p.south).contains(true)))
          }

          val (left, right) = (walk(_.west), walk(_.east))
          val sides = Seq(left.last.west, right.last.east).filterNot(clay.contains)
          val all = left ++ right ++ sides

          if (sides.nonEmpty) flow(springs.tail ++ sides, water ++ all.map(_ -> false))
          else flow(springs.tail + springs.head.north, water ++ all.map(_ -> true))
      }
    }
  }

  private def printMap(water: Map[Coords, Boolean]): Unit = {
    val xRange = (water.keys ++ clay).minBy(_.h).h to (water.keys ++ clay).maxBy(_.h).h
    (smallestY to largestY).foreach { y =>
      println(xRange.map(x =>
        if (clay.contains(Coords(y, x))) '#'
        else if (water.getOrElse(Coords(y, x), false)) '~'
        else if (water.contains(Coords(y, x))) '|'
        else '.').mkString)
    }
  }
//  printMap(Map.empty)

  private val water = flow(Set(source), Map.empty).filter(w => w._1.v >= smallestY && w._1.v <= largestY)
//  printMap(water)

  def one: Int = water.size

  def two: Int = water.count(_._2)
}