package y2015.w2

import common.Day
import common.Utils._

import scala.annotation.tailrec

class Day11 extends Day(inputPath(2015, 11)) {
//  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val data: String = "cqjxjnds"

  private def genNext(old: String): String = {
    def increaseAt(s: String, i: Int): String = {
      val pair = s.splitAt(i)
      val increment = if ("hnk".contains(s(i))) 2 else if (s(i) == 'z') -25 else 1
      pair._1 + (s(i) + increment).toChar + pair._2.tail
    }

    @tailrec
    def increaseRec(s: String, i: Int): String =
      if (s(i) == 'z') {
        if (i == 0) s
        else increaseRec(increaseAt(s, i), i-1)
      } else increaseAt(s, i)

    if (old == "zzzzzzzz") old
    else {
      val badIndex = old.lastIndexWhere("iol".contains(_))
      if (badIndex != -1) old.splitAt(badIndex)._1 + (old(badIndex) + 1).toChar + ("a" * (7-badIndex))
      else increaseRec(old, 7)
    }
  }

  private def check(s: String): Boolean = {
    val sWithIndicesInit = s.zipWithIndex.init
    val pairs = sWithIndicesInit.filter(ci => ci._1 == s(ci._2+1))
    sWithIndicesInit.init.exists(ci => ci._1 >= 'a' && ci._1 <= 'x' && s(ci._2+1) == ci._1+1 && s(ci._2+2) == ci._1+2) &&
      s.forall(!"iol".contains(_)) &&
      pairs.exists(ci1 => pairs.exists(ci2 => ci2._1 != ci1._1 && ci2._2 > ci1._2+1))
  }

  @tailrec
  private def genNCheck(s: String): String = {
    val newS = genNext(s)
    if (check(newS)) newS
    else genNCheck(newS)
  }

  def one: Unit = println(genNCheck(data))

  def two: Unit = println(genNCheck(genNCheck(data)))
}