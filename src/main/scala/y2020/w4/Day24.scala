package y2020.w4

import common.{Coords, Day}
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day24 extends Day(inputPath(2020, 24), testPath(2020, 24, 1)) {
  private val tilesDirections: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  @tailrec
  private def parseDirections(from: Coords, directions: String): Coords = directions match {
    case s if s.startsWith("e") => parseDirections(from + Coords(0, 2), s.substring(1))
    case s if s.startsWith("se") => parseDirections(from + Coords(1, 1), s.substring(2))
    case s if s.startsWith("sw") => parseDirections(from + Coords(1, -1), s.substring(2))
    case s if s.startsWith("w") => parseDirections(from + Coords(0, -2), s.substring(1))
    case s if s.startsWith("nw") => parseDirections(from + Coords(-1, -1), s.substring(2))
    case s if s.startsWith("ne") => parseDirections(from + Coords(-1, 1), s.substring(2))
    case _ => from
  }
  private val tilesToFlip = tilesDirections.map(parseDirections(Coords(0,0), _))
  private val blacks = tilesToFlip.distinct.map((t: Coords) => (t, tilesToFlip.count(_ == t)))
    .filter(_._2 % 2 == 1).map(_._1).toSet

  def one: Int = blacks.size

  private def neighborsOf(t: Coords): Set[Coords] =
    Set(Coords(0,2), Coords(1,1), Coords(1,-1), Coords(0,-2), Coords(-1,-1), Coords(-1,1)).map(_ + t)

  private def applyRules(blacks: Set[Coords]): Set[Coords] = {
    val neighborsOfBlacks = blacks.flatMap(neighborsOf)
    val oldBlacks = blacks.map(t => (t, neighborsOf(t).count(blacks.contains)))
      .filter(p => Set(1,2).contains(p._2)).map(_._1)
    val newBlacks = neighborsOfBlacks.map(t => (t, neighborsOf(t).count(blacks.contains))).filter(_._2 == 2).map(_._1)
    oldBlacks ++ newBlacks
  }

  @tailrec
  private def flipByDay(blacks: Set[Coords], days: Int): Set[Coords] = days match {
    case 0 => blacks
    case _ => flipByDay(applyRules(blacks), days-1)
  }

  def two: Int = flipByDay(blacks, 100).size
}