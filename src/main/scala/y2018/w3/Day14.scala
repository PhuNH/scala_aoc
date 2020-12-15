package y2018.w3

import common.Day

import scala.collection.mutable.ArrayBuffer

class Day14 extends Day("637061") {
  private def run(length: Int): String = {
    var firstIndex = 0
    var secondIndex = 1
    val scores = ArrayBuffer[Byte](3, 7)

    do {
      val sum = scores(firstIndex) + scores(secondIndex)
      scores.addAll(sum.toString.map(_.toString.toByte))
      firstIndex = (firstIndex+1+scores(firstIndex)) % scores.length
      secondIndex = (secondIndex+1+scores(secondIndex)) % scores.length
    } while (scores.length < length+10)
    scores.slice(length, length+10).mkString("")
  }

  def one: Unit = println(run(inputs(0).toInt))

  private def runUntil(recipes: String): Int = {
    var firstIndex = 0
    var secondIndex = 1
    val scores = ArrayBuffer[Byte](3, 7)
    var startIndex = 0

    do {
      do {
        val sum = scores(firstIndex) + scores(secondIndex)
        scores.addAll(sum.toString.map(_.toString.toByte))
        firstIndex = (firstIndex+1+scores(firstIndex)) % scores.length
        secondIndex = (secondIndex+1+scores(secondIndex)) % scores.length
      } while (scores.length < startIndex + recipes.length)
      if (scores.slice(startIndex, startIndex+recipes.length).mkString("") != recipes) startIndex += 1
      else return startIndex
    } while (true)
    0
  }

  def two: Int = runUntil(inputs(0))
}