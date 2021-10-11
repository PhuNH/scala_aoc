package y2019.w4

import common.Day
import common.Utils._

//import scala.annotation.tailrec
import scala.io.Source

case class Deck(length: BigInt) {
  def dealNew(index: BigInt): BigInt = length - index - 1

  def cut(n: BigInt, initialIndex: BigInt): BigInt = (initialIndex + length - n) % length

  def dealInc(n: BigInt, initialIndex: BigInt): BigInt = initialIndex * n % length

  def cutBackwards(n: BigInt, finalIndex: BigInt): BigInt = (finalIndex + length + n) % length

  def dealIncBackwards(n: BigInt, finalIndex: BigInt): BigInt = {
//    @tailrec
//    def findInitialIndex(count: Long): Long = {
//      if ((finalIndex + count * length) % n == 0) (finalIndex + count * length) / n
//      else findInitialIndex(count+1)
//    }
//    findInitialIndex(0)
    n.modInverse(length) * finalIndex % length
  }
}

class Day22 extends Day(inputPath(2019, 22)) {
  private val process: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  def shuffle(deck: Deck, initialIndex: BigInt): BigInt = {
    process.foldLeft(initialIndex)((index, step) => {
      val words = step.split(' ')
      words(0) match {
        case "deal" => words(3).toIntOption match {
          case None => deck.dealNew(index)
          case Some(n) => deck.dealInc(n, index)
        }
        case _ =>
          val n = words(1).toInt
          deck.cut(n, index)
      }
    })
  }

  def one: Unit = {
    println(shuffle(Deck(10007), 2019))
  }

  def shuffleBackwards(deck: Deck, finalIndex: BigInt): BigInt = {
    process.foldRight(finalIndex)((step, index) => {
      val words = step.split(' ')
      words(0) match {
        case "deal" => words(3).toIntOption match {
          case None => deck.dealNew(index)
          case Some(n) => deck.dealIncBackwards(n, index)
        }
        case _ =>
          val n = words(1).toInt
          deck.cutBackwards(n, index)
      }
    })
  }

  def two: Unit = {
    val length = 119315717514047L
    val deck = Deck(length)
    val x = 2020
    val y = shuffleBackwards(deck, x)
    val fy = shuffleBackwards(deck, y)
//    val a = (fy - y) * (y-x).modInverse(length) % length // initial = -117671365094218
    val a = (y - fy) * (x-y).modInverse(length) % length // initial = 1644352419829
    // 1644352419829 + 117671365094218 = length
    val b = (y - a * x) % length
    val times = 101741582076661L
    val initial = (a.modPow(times, length) * x + (a.modPow(times, length)-1) * (a-1).modInverse(length) * b) % length
    println(initial)
  }
}