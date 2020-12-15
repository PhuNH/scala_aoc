package y2018.w3

import common.Day
import common.Utils._

import scala.collection.mutable
import scala.io.Source

class Day12 extends Day(inputPath(2018, 12), testPath(2018, 12, 1)) {
  private val PLANT = '#'
  private val EMPTY = '.'
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val initial = data(0).split(':').apply(1).trim
  private val notes = data.slice(2, data.length).filter(s => s(2) != s(9)).map(s => (s.substring(0,5), s.charAt(9))).toMap

  private def grow(times: Int): Int = {
    val next = new mutable.StringBuilder(initial)
    var current = new StringBuilder()

    // check to add before pot 0
    def checkKeyAtStart(k: String): Int = {
      var i = 4
      while (i > 0) {
        val (left, right) = k.splitAt(i)
        if (!left.contains(PLANT) && next.startsWith(right)) return i
        i -= 1
      }
      i
    }
    // check to add after the last pot
    def checkKeyAtEnd(k: String): Int = {
      var i = 1
      while (i < 5) {
        val (left, right) = k.splitAt(i)
        if (next.endsWith(left) && !right.contains(PLANT)) return 5-i
        i += 1
      }
      5-i
    }

    var indexOfPot0 = 0
    var t = 0
    var sum = 0
    var prevSum = 0
    while (t < times) {
      val potsToPrepend = notes.keys.map(checkKeyAtStart).max
      if (potsToPrepend > 0) {
        next.replace(0, 1, new String(Array.fill(potsToPrepend)(EMPTY)).appended(next(0)))
        indexOfPot0 += potsToPrepend
      }
      val potsToAppend = notes.keys.map(checkKeyAtEnd).max
      if (potsToAppend > 0) next.insert(next.length, new String(Array.fill(potsToAppend)(EMPTY)))
      current = next.clone()
      for (i <- 0 to current.length-5) {
        val keyHere = current.substring(i, i+5)
        if (notes.contains(keyHere)) next.setCharAt(i+2, notes(keyHere))
      }
      t += 1
      sum = next.indices.filter(next(_) == PLANT).map(_ - indexOfPot0).sum
      println(s"$t $sum ${sum - prevSum}")
      prevSum = sum
    }

    sum
  }

  def one: Int = grow(20)

  // from 94th generation, increment is 22
  def two: Long = (50000000000L - 94) * 22 + grow(94)
}