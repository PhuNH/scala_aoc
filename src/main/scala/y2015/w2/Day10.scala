package y2015.w2

import common.Day
import common.Utils._

class Day10 extends Day(inputPath(2015, 10)) {
//  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val data: String = "3113322113"

  private def lookNSay(look: String): String =
    look.foldLeft(List.empty[(Int, Char)])((l, c) => {
      if (l.nonEmpty && l.head._2 == c) (l.head._1+1, l.head._2) :: l.tail
      else (1, c) :: l
    }).reverse.map(ic => ic._1.toString + ic._2).mkString

  def one: Int = (1 to 40).foldLeft(data)((s, _) => lookNSay(s)).length

  def two: Int = (1 to 50).foldLeft(data)((s, _) => lookNSay(s)).length
}