package y2018.w2

import common.Day
import common.Utils._

import scala.collection.mutable.ListBuffer
import scala.io.Source

class Day9 extends Day(inputPath(2018, 9)) {
  private val input: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().next().split(' '))
  private val playerCount = input(0).toInt
  private val lastMarblePoint = input(6).toInt

  private def play(maxMarblePoint: Int, players: Array[Int]): Unit = {
    val circle = ListBuffer(0)
    var current = 0
    val playerCount = players.length

    def toIndex(offset: Int = 0): Int = {
      val newCurrent = current + offset
      if (newCurrent < 0) circle.length + newCurrent
      else if (newCurrent >= circle.length) newCurrent - circle.length
      else newCurrent
    }

    for (marble <- Range.inclusive(1, maxMarblePoint)) marble % 23 match {
      case 0 =>
        val indexToBeRemoved = toIndex(-7)
        val playerId = (marble-1) % playerCount
        players(playerId) += marble + circle(indexToBeRemoved)
        circle.remove(indexToBeRemoved)
        current = indexToBeRemoved
        if (current == circle.length) current = 0
      case _ =>
        val before = toIndex(1)
        circle.insert(before+1, marble)
        current = before+1
    }
  }

  def one: Int = {
    val players = Array.fill(playerCount)(0)
    play(lastMarblePoint, players)
    players.max
  }

  def two: Int = {
    val players = Array.fill(playerCount)(0)
    play(lastMarblePoint*100, players)
    players.max
  }
}
