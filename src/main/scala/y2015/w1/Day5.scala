package y2015.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day5 extends Day(inputPath(2015, 5)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def isNice(s: String): Boolean = {
    s.count("aeiou".contains(_)) >= 3 &&
      s.zipWithIndex.init.exists(p => s(p._2+1) == p._1) &&
      !(s.contains("ab") || s.contains("cd") || s.contains("pq") || s.contains("xy"))
  }

  private def isNice2(s: String): Boolean = {
    val sWithIndexInit = s.zipWithIndex.init.init
    sWithIndexInit.exists(p => s.substring(p._2+2).contains(s.substring(p._2, p._2+2))) &&
      sWithIndexInit.exists(p => s(p._2+2) == p._1)
  }

  def one: Int = data.count(isNice)

  def two: Int = data.count(isNice2)
}