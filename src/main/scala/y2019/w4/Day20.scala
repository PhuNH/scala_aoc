package y2019.w4

import common.Utils._
import common.{Coords, Day}

import scala.collection.mutable
import scala.io.Source

class Day20 extends Day(inputPath(2019, 20)) {
  private val Passage = '.'

  private val passages = mutable.Set.empty[Coords]
  private val portalChars = mutable.Map.empty[Coords, Char]

  using(Source.fromResource(inputs(0)))(_.getLines().zipWithIndex.foreach { case (l, y) =>
    l.zipWithIndex.foreach { case (char, x) =>
      val coords = Coords(y, x)
      char match {
        case Passage => passages.add(coords)
        case char =>
          if (char >= 'A' && char <= 'Z') portalChars.addOne(coords, char)
          else ()
      }
    }
  })
  private val portalNamesWithCoords = passages.filter(_.adjacents.exists(adjacent => portalChars.contains(adjacent)))
    .map(coords => {
      val portalName = {
        if (portalChars.contains(coords.north)) List(portalChars(coords.north.north), portalChars(coords.north))
        else if (portalChars.contains(coords.east)) List(portalChars(coords.east), portalChars(coords.east.east))
        else if (portalChars.contains(coords.south)) List(portalChars(coords.south), portalChars(coords.south.south))
        else List(portalChars(coords.west.west), portalChars(coords.west))
      }
      (coords, portalName)
    })
  private val portals = portalNamesWithCoords.groupBy(_._2).view.mapValues(_.map(_._1)).toMap.filter(_._2.size == 2)
    .flatMap { case (_, coordsSet) =>
      val iter = coordsSet.iterator
      val first = iter.next()
      val second = iter.next()
      mutable.ArraySeq((first, second), (second, first))
    }
  private val start = portalNamesWithCoords.find(_._2 == List('A', 'A')).get._1
  private val end = portalNamesWithCoords.find(_._2 == List('Z', 'Z')).get._1
  private val eastmost = portalNamesWithCoords.maxBy(_._1.h)._1.h
  private val southmost = portalNamesWithCoords.maxBy(_._1.v)._1.v
  private val portalPartitions = portals.keys.partition(coords =>
    coords.v == 2 || coords.v == southmost || coords.h == 2 || coords.h == eastmost)
  private val outerPortalSet = portalPartitions._1.toSet
  private val innerPortalSet = portalPartitions._2.toSet

  case class PathSearchUnit(coords: Coords, length: Int)

  def bfs(src: Coords, target: Coords): Int = {
    val searchQueue = mutable.Queue.empty[PathSearchUnit]
    searchQueue.enqueue(PathSearchUnit(src, 0))
    val searched = mutable.Set.empty[Coords]
    searched.add(src)

    do {
      val currentUnit = searchQueue.dequeue()
      if (target == currentUnit.coords) return currentUnit.length

      val possibleNextSteps =
        if (!portals.contains(currentUnit.coords)) currentUnit.coords.adjacents
        else currentUnit.coords.adjacents.appended(portals(currentUnit.coords))
      searchQueue.enqueueAll(possibleNextSteps
        .filter(coords => !searched.contains(coords) && passages.contains(coords))
        .map(coords => {
          searched.add(coords)
          PathSearchUnit(coords, currentUnit.length+1)
        }))
    } while (searchQueue.nonEmpty)
    0
  }

  def one: Int = {
    bfs(start, end)
  }

  case class CoordsWithLayer(coords: Coords, layer: Int)
  case class PathSearchUnitWithLayer(coords: Coords, layer: Int, length: Int)

  def bfsWithLayer(src: Coords, target: Coords): Int = {
    val searchQueue = mutable.Queue.empty[PathSearchUnitWithLayer]
    searchQueue.enqueue(PathSearchUnitWithLayer(src, 0, 0))
    val searched = mutable.Set.empty[CoordsWithLayer]
    searched.add(CoordsWithLayer(src, 0))

    do {
      val currentUnit = searchQueue.dequeue()
      if (currentUnit.layer == 0 && target == currentUnit.coords) return currentUnit.length

      val possibleAdjacentNextSteps = currentUnit.coords.adjacents.map(CoordsWithLayer(_, currentUnit.layer))
      val possibleNextSteps =
        if (!portals.contains(currentUnit.coords)) possibleAdjacentNextSteps
        else if (outerPortalSet.contains(currentUnit.coords))
          possibleAdjacentNextSteps.appended(CoordsWithLayer(portals(currentUnit.coords), currentUnit.layer-1))
        else possibleAdjacentNextSteps.appended(CoordsWithLayer(portals(currentUnit.coords), currentUnit.layer+1))
      searchQueue.enqueueAll(possibleNextSteps
        .filter(coordsWithLayer => !searched.contains(coordsWithLayer) && passages.contains(coordsWithLayer.coords) &&
          (if (coordsWithLayer.layer == 0) !outerPortalSet.contains(coordsWithLayer.coords)
          else coordsWithLayer.coords != start && coordsWithLayer.coords != end))
        .map(coordsWithLayer => {
          searched.add(coordsWithLayer)
          PathSearchUnitWithLayer(coordsWithLayer.coords, coordsWithLayer.layer, currentUnit.length+1)
        }))
    } while (searchQueue.nonEmpty)
    0
  }

  def two: Int = {
    bfsWithLayer(start, end)
  }
}