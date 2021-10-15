package y2018.w4

import common.Utils._
import common.{Coords, Day}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

class Day20 extends Day(inputPath(2018, 20)) {
  private val symbol2Direction = Map('W' -> Coords.West, 'S' -> Coords.South, 'E' -> Coords.East, 'N' -> Coords.North)
  private val rootData: String = using(Source.fromResource(inputs(0)))(_.getLines().next())
  private val root = Coords(0,0)
  private val grids = mutable.Map(root -> mutable.Set.empty[Coords])
  private val searchStack = mutable.Stack.empty[Coords]

  @tailrec
  private def processData(pos: Coords, index: Int): Unit = rootData(index) match {
    case symbol if symbol2Direction.contains(symbol) =>
      val direction = symbol2Direction(symbol)
      grids(pos).add(direction)
      grids.getOrElseUpdate(pos + direction, mutable.Set.empty).add(-direction)
      processData(pos + direction, index + 1)
    case '(' =>
      searchStack.push(pos)
      processData(pos, index + 1)
    case '|' => processData(searchStack.top, index + 1)
    case ')' => processData(searchStack.pop(), index + 1)
    case _ =>
  }
  processData(root, 1)

  private def bfsLongest(src: Coords): (Int, Int) = {
    val searchQueue = mutable.Queue.empty[(Coords, Int)]
    searchQueue.enqueue((src, 0))
    val searched = mutable.Map.empty[Coords, Int]
    searched.addOne(src, 0)

    do {
      val currentUnit = searchQueue.dequeue()
      searchQueue.enqueueAll(grids(currentUnit._1).map(currentUnit._1 + _).filterNot(searched.contains).map(nextPos => {
        val posAndLen = (nextPos, currentUnit._2 + 1)
        searched.addOne(posAndLen)
        posAndLen
      }))
      if (searchQueue.isEmpty)
        return (currentUnit._2, searched.count(_._2 >= 1000))
    } while (searchQueue.nonEmpty)
    (0, 0)
  }
  private val (longest, thousands) = bfsLongest(root)

  def one: Int = longest

  def two: Int = thousands
}