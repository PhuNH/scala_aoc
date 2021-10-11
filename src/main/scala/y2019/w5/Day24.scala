package y2019.w5

import common.Utils._
import common.{Coords, Day}

import scala.collection.mutable
import scala.io.Source

class Day24 extends Day(inputPath(2019, 24)) {
  private val initialBugs: Set[Coords] = using(Source.fromResource(inputs(0)))(_.getLines().zipWithIndex
    .flatMap { case (l, y) =>
      l.zipWithIndex.filter(_._1 == '#').map { case (_, x) => Coords(y, x) }
    }.toSet)

  private def develop(bugs: Set[Coords]): Set[Coords] = {
    val spaces2Bugs = bugs.flatMap(_.adjacents.filter(c => c.v < 5 && c.h < 5 && c.v >= 0 && c.h >= 0))
      .diff(bugs).filter(space => {
        val adjacentBugs = space.adjacents.count(bugs.contains)
        adjacentBugs == 1 || adjacentBugs == 2
      })
    val bugs2Bugs = bugs.filter(bug => bug.adjacents.count(bugs.contains) == 1)
    bugs2Bugs union spaces2Bugs
  }

  private def biodiversity(bugs: Set[Coords]): Long = {
    bugs.map(c => math.pow(2, c.h + c.v * 5).toLong).sum
  }

  def one: Long = {
    val layouts = mutable.Set.empty[Set[Coords]]
    var next = initialBugs
    do {
      next = develop(next)
      if (layouts.contains(next)) return biodiversity(next)
      else layouts.add(next)
    } while (true)
    0
  }

  private case class LayeredCoords(coords: Coords, layer: Int)

  private def layeredNeighbors(tile: LayeredCoords): Set[LayeredCoords] = {
    val neighbors = mutable.Set.empty[LayeredCoords]
    // left
    if (tile.coords.h == 0) neighbors.add(LayeredCoords(Coords(2,1), tile.layer-1))
    else if (tile.coords == Coords(2,3))
      neighbors.addAll((0 to 4).map(y => LayeredCoords(Coords(y, 4), tile.layer+1)))
    else neighbors.add(LayeredCoords(tile.coords.west, tile.layer))
    // right
    if (tile.coords.h == 4) neighbors.add(LayeredCoords(Coords(2,3), tile.layer-1))
    else if (tile.coords == Coords(2,1))
      neighbors.addAll((0 to 4).map(y => LayeredCoords(Coords(y, 0), tile.layer+1)))
    else neighbors.add(LayeredCoords(tile.coords.east, tile.layer))
    // above
    if (tile.coords.v == 0) neighbors.add(LayeredCoords(Coords(1,2), tile.layer-1))
    else if (tile.coords == Coords(3,2))
      neighbors.addAll((0 to 4).map(x => LayeredCoords(Coords(4, x), tile.layer+1)))
    else neighbors.add(LayeredCoords(tile.coords.north, tile.layer))
    // below
    if (tile.coords.v == 4) neighbors.add(LayeredCoords(Coords(3,2), tile.layer-1))
    else if (tile.coords == Coords(1,2))
      neighbors.addAll((0 to 4).map(x => LayeredCoords(Coords(0, x), tile.layer+1)))
    else neighbors.add(LayeredCoords(tile.coords.south, tile.layer))
    neighbors.toSet
  }

  private def developRecursively(bugs: Set[LayeredCoords]): Set[LayeredCoords] = {
    val spaces2Bugs = bugs.flatMap(layeredNeighbors)
      .diff(bugs).filter(space => {
      val adjacentBugs = layeredNeighbors(space).count(bugs.contains)
      adjacentBugs == 1 || adjacentBugs == 2
    })
    val bugs2Bugs = bugs.filter(bug => layeredNeighbors(bug).count(bugs.contains) == 1)
    bugs2Bugs union spaces2Bugs
  }

  def two: Long = {
    var next = initialBugs.map(c => LayeredCoords(c, 0))
    for (_ <- 1 to 200) next = developRecursively(next)
    next.size
  }
}