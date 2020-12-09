package y2018.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day9 extends Day(inputPath(2018, 9)) {
  private val input: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().next().split(' '))
  private val playerCount = input(0).toInt
  private val lastMarblePoint = input(6).toInt

  class CircularList[T]() {
    case class Node(var element: T) {
      var next: Node = null
      var prev: Node = null
    }

    private var head: Node = null
    private var current = head

    def insert(e: T): Unit = {
      if (head == null) {
        head = Node(e)
        head.next = head
        head.prev = head
        current = head
      } else {
        val newNode = Node(e)
        newNode.next = current.next
        newNode.prev = current
        current.next.prev = newNode
        current.next = newNode
        current = newNode
      }
    }

    def remove(): Unit = {
      current.prev.next = current.next
      current.next.prev = current.prev
      current = current.next
    }

    def move(offset: Int): Unit = {
      var i = offset
      while (i < 0) {
        current = current.prev
        i += 1
      }
      while (i > 0) {
        current = current.next
        i -= 1
      }
    }

    def apply(): T = current.element

    override def toString: String = {
      var t = head
      var s = ""
      do {
        s += t.element + " "
        t = t.next
      } while (t != head)
      s
    }
  }

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
