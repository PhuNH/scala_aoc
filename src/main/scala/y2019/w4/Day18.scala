package y2019.w4

import common.Utils._
import common.{Coords, Day}

import scala.collection.mutable
import scala.io.Source

class Day18 extends Day(inputPath(2019, 18)) {
  private val Entrance = '@'
  private val Passage = '.'

  private val entrances = mutable.Set.empty[Coords]
  private val passages = mutable.Set.empty[Coords]
  private val keys = mutable.Map.empty[Coords, Char]
  private val doors = mutable.Map.empty[Coords, Char]

  using(Source.fromResource(inputs(0)))(_.getLines().zipWithIndex.foreach { case (l, y) =>
    l.zipWithIndex.foreach {
      case (char, x) =>
        val coord = Coords.apply(v = y, h = x)
        char match {
          case Entrance => entrances.add(coord)
          case Passage => passages.add(coord)
          case char =>
            if (char >= 'a' && char <= 'z') keys.addOne(coord, char)
            else if (char >= 'A' && char <= 'Z') doors.addOne(coord, char)
            else ()
        }
    }
  })
  private val keyNames = keys.values.toSet

  private case class PathSearchUnit(coords: Coords, doors: Set[Char], length: Int)

  // TODO compare time between stack and queue
  private def searchPaths(src: Coords, targets: Set[Coords]): Array[PathSearchUnit] = {
    var paths = Array.empty[PathSearchUnit]

    val searchQueue = mutable.Queue.empty[PathSearchUnit]
    searchQueue.enqueue(PathSearchUnit(src, Set.empty[Char], 0))
    val searched = mutable.Set.empty[Coords]
    searched.add(src)

    do {
      val currentUnit = searchQueue.dequeue()
      if (targets.contains(currentUnit.coords))
        paths = paths.appended(currentUnit)
      searchQueue.enqueueAll(currentUnit.coords.adjacents
        .filter(coords => !searched.contains(coords) &&
          (entrances.contains(coords) || passages.contains(coords) || keys.contains(coords) || doors.contains(coords)))
        .map(coords => {
          searched.add(coords)
          val newDoors = if (doors.contains(coords)) currentUnit.doors.incl(doors(coords)) else currentUnit.doors
          PathSearchUnit(coords, newDoors, currentUnit.length+1)
        }))
    } while (searchQueue.nonEmpty)
    paths
  }

  private def findShortestLen(src: Coords, paths: Map[Coords, Array[PathSearchUnit]]): Int = {
    val states = mutable.Map.empty[(Coords, Set[Char]), Int]

    def traverse(src: Coords, collected: Set[Char], accLen: Int): Int = {
      if (collected == keyNames) accLen
      else if (!states.contains((src, collected)) || accLen < states((src, collected))) {
        states.update((src, collected), accLen)

        val possiblePaths = paths(src).filter(psu =>
          !collected.contains(keys(psu.coords)) && psu.doors.forall(door => collected.contains(door.toLower)))
        if (possiblePaths.isEmpty) Int.MaxValue
        else possiblePaths.map(psu => {
          val newCollected = collected.incl(keys(psu.coords))
          val newAccLen = accLen + psu.length
          traverse(psu.coords, newCollected, newAccLen)
        }).min
      }
      else Int.MaxValue
    }

    traverse(src, Set.empty[Char], 0)
  }

  def one: Int = {
    val paths = keys.keys.map(k => (k, searchPaths(k, keys.keys.toSet.excl(k)))).toMap
      .updated(entrances.head, searchPaths(entrances.head, keys.keys.toSet))
    findShortestLen(entrances.head, paths)
  }

  private def findShortestCombinedLen(srcs: Set[Coords], paths: Map[Coords, Array[PathSearchUnit]]): Int = {
    val overallStates = mutable.Map.empty[(Map[Coords, Coords], Set[Char]), Int]

    def traverse(srcs: Map[Coords, Coords], collected: Set[Char], accLen: Int): Int = {
      if (collected == keyNames) accLen
      else if (!overallStates.contains((srcs, collected)) || accLen < overallStates((srcs, collected))) {
        overallStates.update((srcs, collected), accLen)

        val overallPossiblePaths = srcs.map(src => {
          val possiblePaths = paths(src._2).filter(psu =>
            !collected.contains(keys(psu.coords)) && psu.doors.forall(door => collected.contains(door.toLower)))
          possiblePaths.map(psu => (src._1, Some(psu))).appended((src._1, None))
        }).toArray
        val possibilities = for {
          a <- overallPossiblePaths(0)
          b <- overallPossiblePaths(1)
          c <- overallPossiblePaths(2)
          d <- overallPossiblePaths(3)
          if a._2.nonEmpty || b._2.nonEmpty || c._2.nonEmpty || d._2.nonEmpty
        } yield {
          Set(a, b, c, d)
        }
        possibilities.map(quarters => {
          val newSrcs = quarters.map(quarter => {
            (quarter._1, quarter._2 match {
              case None => srcs(quarter._1)
              case Some(psu) => psu.coords
            })
          }).toMap
          val moreCollected = quarters.filter(_._2.nonEmpty).map {
            case (_, psuOpt) => keys(psuOpt.get.coords)
          }
          val newCollected = collected.concat(moreCollected)
          val moreLen = quarters.filter(_._2.nonEmpty).toArray.map {
            case (_, psuOpt) => psuOpt.get.length
          }.sum
          val newAccLen = accLen + moreLen
          traverse(newSrcs, newCollected, newAccLen)
        }).min
      }
      else Int.MaxValue
    }

    traverse(srcs.map(src => (src, src)).toMap, Set.empty[Char], 0)
  }

  def two: Int = {
    val oldEntrance = entrances.head
    entrances.remove(oldEntrance)
    entrances.addAll(oldEntrance.diagonals)
    oldEntrance.adjacents.foreach(p => passages.remove(p))

    val paths = keys.keys.map(k => (k, searchPaths(k, keys.keys.toSet.excl(k)))).toMap ++
      entrances.map(coords => (coords, searchPaths(coords, keys.keys.toSet)))
    findShortestCombinedLen(entrances.toSet, paths)
  }
}
