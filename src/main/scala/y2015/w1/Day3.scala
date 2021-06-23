package y2015.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day3 extends Day(inputPath(2015, 3)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val moves: String = data(0)

  private def move(m: Char, location: (Int, Int)): (Int, Int) = m match {
    case '^' => (location._1, location._2+1)
    case 'v' => (location._1, location._2-1)
    case '>' => (location._1+1, location._2)
    case _ => (location._1-1, location._2)
  }

  def one: Int = {
    var location = (0, 0)
    var houses = Set(location)
    for (m <- moves) {
      location = move(m, location)
      if (!houses.contains(location)) houses += location
    }
    houses.size
  }

  def two: Int = {
    var santaLoc = (0, 0)
    var roboLoc = (0, 0)
    var santaHouses = Set(santaLoc)
    var roboHouses = Set(roboLoc)
    for ((m, i) <- moves.zipWithIndex) {
      if (i % 2 == 0) {
        santaLoc = move(m, santaLoc)
        if (!santaHouses.contains(santaLoc) && !roboHouses.contains(santaLoc)) santaHouses += santaLoc
      } else {
        roboLoc = move(m, roboLoc)
        if (!santaHouses.contains(roboLoc) && !roboHouses.contains(roboLoc)) roboHouses += roboLoc
      }
    }
    santaHouses.size + roboHouses.size - 1
  }
}