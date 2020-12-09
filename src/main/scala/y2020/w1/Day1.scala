package y2020.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day1 extends Day(inputPath(2020, 1)) {
  private val entries: List[Int] = using(Source.fromResource(inputs(0)))(_.getLines().toList.map(_.trim.toInt))

  def one: Int = {
    @scala.annotation.tailrec
    def findSum2020(list: List[Int]): Int = {
      list.tail.map(_ + list.head).find(_ == 2020) match {
        case Some(_) => list.head * (2020 - list.head)
        case None => findSum2020(list.tail)
      }
    }
    findSum2020(entries)
  }

  def two: Long = {
    (for {
      i <- Range(0, entries.length-2)
      j <- Range(i+1, entries.length-1)
      k <- Range(j+1, entries.length)
      if entries(i) + entries(j) + entries(k) == 2020
    } yield entries(i) * entries(j) * entries(k)).head
  }
}
