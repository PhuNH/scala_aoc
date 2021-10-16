package y2018.w4

import common.{Coords, Day}
import common.Utils._

import scala.collection.mutable
import scala.io.Source

class Day22 extends Day(inputPath(2018, 22)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().map(_.split(' ')(1)).toArray)
  private val depth = data(0).toInt
  private val target = {
    val temp = data(1).split(',').map(_.toInt)
    Coords(temp(1), temp(0))
  }
  private val mouth = Coords(0,0)
  private val erosionLevels = mutable.Map.empty[Coords, Int]

  private object Type extends Enumeration {
    val Rocky: Type.Value = Value
    val Wet: Type.Value = Value
    val Narrow: Type.Value = Value
  }

  private def geologicIndex(region: Coords): Int = region match {
    case r if r == mouth || r == target => 0
    case r if r.v == 0 => r.h * 16807
    case r if r.h == 0 => r.v * 48271
    case r => erosionLevel(Coords(r.v, r.h-1)) * erosionLevel(Coords(r.v-1, r.h))
  }

  private def erosionLevel(region: Coords): Int =
    if (erosionLevels.contains(region)) erosionLevels(region)
    else {
      val result = (geologicIndex(region) + depth) % 20183
      erosionLevels.addOne(region, result)
      result
    }

  private def regionType(region: Coords) = erosionLevel(region) % 3 match {
    case 0 => Type.Rocky
    case 1 => Type.Wet
    case _ => Type.Narrow
  }

  private def riskLevel(target: Coords): Int = (for {
    x <- 0 to target.h
    y <- 0 to target.v
  } yield erosionLevel(Coords(y, x)) % 3).sum

  def one: Int = riskLevel(target)

  private def toolsOfRegion(region: Coords): Set[Int] = regionType(region) match {
    case Type.Rocky => Set(1, 2)
    case Type.Wet => Set(0, 2)
    case _ => Set(0, 1)
  }

  // tools: 0 -> none, 1 -> torch, 2 -> gear
  private case class Node(region: Coords, tool: Int) {
    def neighbors: Map[Node, Int] = {
      val toolChangeNode = Node(region, toolsOfRegion(region).excl(tool).head)
      val toolChangeNeighbor = Map(toolChangeNode -> 7)
      val moveNodes = region.adjacents.filter(a => a.v >= 0 && a.h >= 0 && toolsOfRegion(a).contains(tool))
        .map(Node(_, tool))
      val moveNeighbors = moveNodes.map((_, 1)).toMap
      toolChangeNeighbor ++ moveNeighbors
    }

    def unvisitedNeighbors(visited: mutable.Map[Node, Int]): Map[Node, Int] = {
      neighbors.filterNot(n => visited.contains(n._1))
    }

    def unsearchedNeighbors(searched: mutable.Set[NodeDist]): Map[Node, Int] = {
      neighbors.filterNot(n => searched.contains(NodeDist(n)))
    }
  }

  private case class NodeDist(node: Node, var dist: Int)
  private object NodeDist {
    def apply(tuple: (Node, Int)): NodeDist = new NodeDist(tuple._1, tuple._2)
  }

  private val visited = mutable.Map.empty[Node, Int]
  private var unvisitedBrowsed = mutable.PriorityQueue.empty[NodeDist](Ordering.by[NodeDist, Int](_.dist).reverse)

  private def dijkstraShortest(src: Node, target: Node): Int = {
    unvisitedBrowsed.enqueue(NodeDist(src, 0))

    while (unvisitedBrowsed.nonEmpty) {
      val currentNodeDist = unvisitedBrowsed.dequeue()
      if (currentNodeDist.node == target) return currentNodeDist.dist

      currentNodeDist.node.unvisitedNeighbors(visited).foreach(n => {
        val newDist = currentNodeDist.dist + n._2
        unvisitedBrowsed.find(_.node == n._1) match {
          case None => unvisitedBrowsed.enqueue(NodeDist(n._1, newDist))
          case Some(nodeDist) => if (nodeDist.dist > newDist) {
//            nodeDist.dist = newDist
//            unvisitedBrowsed.enqueue(nodeDist)
//            unvisitedBrowsed = unvisitedBrowsed.clone()
            unvisitedBrowsed = unvisitedBrowsed.partition(_ == nodeDist)._2
            unvisitedBrowsed.enqueue(NodeDist(nodeDist.node, newDist))
          }
        }
      })
      visited.addOne(currentNodeDist.node, currentNodeDist.dist)
    }
    0
  }

  // NodeDist used here is not the same as the one used in dijkstraShortest:
  // - Here "dist" is edge length
  // - In dijkstraShortest "dist" is distance to source
  private def bfsShortest(src: Node, target: Node): Int = {
    val nodeDist = NodeDist(src, 1)
    val searchQueue = mutable.Queue.empty[(NodeDist, Int)]
    searchQueue.enqueue((nodeDist, 0))
    val searched = mutable.Set.empty[NodeDist]
    searched.addOne(nodeDist)

    do {
      val current = searchQueue.dequeue()

      if (current._1.dist > 1) {
        val nodeDist = NodeDist(current._1.node, current._1.dist-1)
        searchQueue.enqueue((nodeDist, current._2+1))
        searched.addOne(nodeDist)
      } else {
        if (current._1.node == target) return current._2
        val possibleNextSteps = current._1.node.unsearchedNeighbors(searched)
        searched.addAll(possibleNextSteps.map(s => NodeDist(s)))
        searchQueue.enqueueAll(possibleNextSteps.map(s => (NodeDist(s), current._2+1)))
      }
    } while (searchQueue.nonEmpty)
    0
  }

  def two: Int = bfsShortest(Node(mouth, 1), Node(target, 1))
}