package y2015.w2

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day12 extends Day(inputPath(2015, 12)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val red = ":\"red\""

  def one: Int = data(0).split("[],{}:".toCharArray).map(s => s.toIntOption.getOrElse(0)).sum

  @tailrec
  private def findOpen(s: String, from: Int, leftover: Int): Int = {
    val lastOpen = s.lastIndexOf('{', from)
    val closeCount = s.substring(lastOpen, from).count(_ == '}') + leftover
    if (closeCount == 0) lastOpen
    else findOpen(s, lastOpen-1, closeCount-1)
  }

  @tailrec
  private def findClose(s: String, from: Int, leftover: Int): Int = {
    val firstClose = s.indexOf('}', from)
    val openCount = s.substring(from, firstClose).count(_ == '{') + leftover
    if (openCount == 0) firstClose
    else findClose(s, firstClose+1, openCount-1)
  }

  @tailrec
  private def replace(s: String, from: Int): String = {
    val redIndex = s.indexOf(red, from)
    if (redIndex == -1) s
    else {
      val open = findOpen(s, redIndex, 0)
      val baOpen = s.splitAt(open)
      val close = findClose(s, redIndex, 0)
      val baClose = s.splitAt(close+1)
      replace(baOpen._1 + baClose._2, open)
    }
  }

  def two: Int = replace(data(0), 0).split("[],{}:".toCharArray).map(s => s.toIntOption.getOrElse(0)).sum
}