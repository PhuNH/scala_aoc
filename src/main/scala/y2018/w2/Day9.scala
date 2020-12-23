package y2018.w2

import common.{CircularList, Day}
import common.Utils._

import scala.io.Source

class Day9 extends Day(inputPath(2018, 9)) {
  private val input: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().next().split(' '))
  private val playerCount = input(0).toInt
  private val lastMarblePoint = input(6).toInt

  private def play(maxMarblePoint: Int, players: Array[Long]): Unit = {
    val circle = new CircularList[Int]
    circle.insert(0)
    val playerCount = players.length

    for (marble <- Range.inclusive(1, maxMarblePoint)) marble % 23 match {
      case 0 =>
        circle.move(-7)
        val playerId = (marble-1) % playerCount
        players(playerId) += marble + circle()
        circle.remove()
      case _ =>
        circle.move(1)
        circle.insert(marble)
    }
  }

  def one: Long = {
    val players = Array.fill(playerCount)(0L)
    play(lastMarblePoint, players)
    players.max
  }

  def two: Long = {
    val players = Array.fill(playerCount)(0L)
    play(lastMarblePoint*100, players)
    players.max
  }
}
