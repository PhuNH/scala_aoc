package y2018.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day2 extends Day(inputPath(2018, 2)) {
  private val NEITHER = 1
  private val TWICE = 2
  private val THRICE = 3
  private val BOTH = 6

  private val ids: List[String] = using(Source.fromResource(inputs(0)))(_.getLines().toList)

  private def isTwiceThrice(string: String): Int = {
    val occs = string.toSet.toList.map((c: Char) => string.count(_ == c))
    if (occs.contains(2) && occs.contains(3)) BOTH
    else if (occs.contains(3)) THRICE
    else if (occs.contains(2)) TWICE
    else NEITHER
  }

  def one: Int = {
    val times = ids.map(isTwiceThrice)
    val counts = (times.count(_ == TWICE), times.count(_ == THRICE), times.count(_ == BOTH))
    (counts._1 + counts._3) * (counts._2 + counts._3)
  }

  private def compare(s1: String, s2: String): Int = {
    val comparisons = s1.indices.map(i => if (s1(i) == s2(i)) 0 else 1)
    val sum = comparisons.sum
    if (sum == 1) comparisons.length + 1 + comparisons.indices.find(comparisons(_) == 1).get
    else sum
  }

  def two: Int = {
    val iCom = (for {
      i <- Range(0, ids.length-1)
      j <- Range(i+1, ids.length)
      comparison = compare(ids(i), ids(j))
      if comparison > ids(i).length
    } yield (i,comparison)).head
    val diffIndex = iCom._2 - 1 - ids.head.length
    println(ids(iCom._1).substring(0,diffIndex) + ids(iCom._1).substring(diffIndex+1))
    0
  }
}
