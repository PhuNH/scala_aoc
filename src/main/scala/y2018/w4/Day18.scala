package y2018.w4

import common.{Coords, Day}
import common.Utils._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

class Day18 extends Day(inputPath(2018, 18)) {
  private val Tree = '|'
  private val Yard = '#'
  private val Open = '.'

  private val occupieds: Set[(Coords, Char)] = using(Source.fromResource(inputs(0)))(
    _.getLines().zipWithIndex.flatMap { case (l, y) =>
      l.zipWithIndex.flatMap { case (char, x) =>
        if (char != Open) Some((Coords(y, x), char)) else None
      }
    }.toSet)
  private val (treeCoords, yardCoords) = occupieds.partition(_._2 == Tree)
  private val trees = treeCoords.map(_._1)
  private val yards = yardCoords.map(_._1)
  private val states = mutable.Map.empty[(Set[Coords], Set[Coords]), Int]

  private def neighbors(c: Coords): Set[Coords] =
    (c.adjacents ++ c.diagonals).toSet.filter(n => n.v >= 0 && n.h >= 0 && n.v < 50 && n.h < 50)

  private def printArea(trees: Set[Coords], yards: Set[Coords]): Unit = {
    for (i <- 0 to 49) {
      for (j <- 0 to 49) {
        val c = Coords(i, j)
        if (trees.contains(c)) print('|')
        else if (yards.contains(c)) print('#')
        else print('.')
      }
      println()
    }
  }

  @tailrec
  private def change(trees: Set[Coords], yards: Set[Coords], times: Int): (Set[Coords], Set[Coords]) = {
//    if (!states.contains((trees, yards))) states.addOne((trees, yards), times)
//    else println(s"$times repeated from ${states((trees, yards))}")
    // the sequence from 999999552 to 999999525 is repeated
    if (times == 0) (trees, yards)
    else {
      val treesFromOpens = trees.flatMap(neighbors).filter(n =>
        !trees.contains(n) && !yards.contains(n) && neighbors(n).count(trees.contains) >= 3)
      val yardsFromTrees = yards.flatMap(neighbors).filter(n =>
        trees.contains(n) && neighbors(n).count(yards.contains) >= 3)
      val yardsFromYards = yards.filter(y =>
        neighbors(y).count(yards.contains) >= 1 && neighbors(y).count(trees.contains) >= 1)
      change(trees diff yardsFromTrees union treesFromOpens, yardsFromYards union yardsFromTrees, times - 1)
    }
  }

  def one: Int = {
    val (t, y) = change(trees, yards, 10)
    t.size * y.size
  }

  def two: Int = {
    val total = 1000000000
    val repeatStart = 999999552
    val repeatEnd = 999999525
    val beginningNumber = total - repeatStart
    val (t, y) = change(trees, yards, beginningNumber + repeatStart % 28)
    t.size * y.size
  }
}

