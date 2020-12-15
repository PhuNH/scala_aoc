package y2018.w3

import common.{Coords, Day, Direction}
import common.Utils._

import scala.io.Source

class Day13 extends Day(inputPath(2018, 13),
  testPath(2018, 13, 1), testPath(2018, 13, 2)) {
  // test 2: from https://www.reddit.com/r/adventofcode/comments/a5t7vx/2018_day_13_part_2_dont_know_where_the_error/
  case class State(coords: Coords, direction: Direction.Value, directionalTurn: Int)

  private var carts = Array.empty[State]
  private val map: Array[Array[Char]] = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(_.toCharArray))
  for (i <- map.indices) for (j <- map(0).indices) {
    val str = map(i)(j).toString
    if (str == Direction.North.toString || str == Direction.South.toString) {
      carts :+= State(Coords(i, j), Direction.withName(str), 0)
      map(i)(j) = '|'
    } else if (str == Direction.East.toString || str == Direction.West.toString) {
      carts :+= State(Coords(i, j), Direction.withName(str), 0)
      map(i)(j) = '-'
    }
  }

  def run(kind: Int): Unit = {
    var currentCarts = carts
    do {
      currentCarts = currentCarts.sortWith((c1, c2) =>
        c1.coords.v < c2.coords.v || (c1.coords.v == c2.coords.v && c1.coords.h < c2.coords.h))
      var i = 0
      while (i < currentCarts.length) {
        val newCoords = currentCarts(i).coords + Direction.valueToVector(currentCarts(i).direction)
        // a cart can also crash with ones that haven't moved, not only ones that have moved
        val toBeCrashed = currentCarts.indexWhere(_.coords == newCoords)
        if (toBeCrashed >= 0) {
          println(s"${newCoords.h},${newCoords.v}")
          if (kind == 1) return
          else currentCarts = currentCarts.indices.filter(j => j != i && j != toBeCrashed).map(currentCarts).toArray
          if (toBeCrashed < i) i -= 1
        } else {
          var newDirectionalTurn = currentCarts(i).directionalTurn
          val newDirection = map(newCoords.v)(newCoords.h) match {
            case '/' =>
              if (currentCarts(i).direction == Direction.North || currentCarts(i).direction == Direction.South)
                Direction.rightFrom(currentCarts(i).direction)
              else Direction.leftFrom(currentCarts(i).direction)
            case '\\' =>
              if (currentCarts(i).direction == Direction.North || currentCarts(i).direction == Direction.South)
                Direction.leftFrom(currentCarts(i).direction)
              else Direction.rightFrom(currentCarts(i).direction)
            case '+' =>
              newDirectionalTurn = (currentCarts(i).directionalTurn + 1) % 3
              if (currentCarts(i).directionalTurn == 0) Direction.leftFrom(currentCarts(i).direction)
              else if (currentCarts(i).directionalTurn == 2) Direction.rightFrom(currentCarts(i).direction)
              else currentCarts(i).direction
            case _ => currentCarts(i).direction
          }
          currentCarts(i) = State(newCoords, newDirection, newDirectionalTurn)
          i += 1
        }
      }
      if (currentCarts.length == 1) {
        println(s"Last: ${currentCarts(0).coords.h},${currentCarts(0).coords.v}")
        return
      }
    } while (true)
  }

  def one: Unit = run(1)

  def two: Unit = run(2)
}