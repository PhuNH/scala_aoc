package y2020.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day16 extends Day(inputPath(2020, 16)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val firstBlankLine = data.indexWhere(_ == "")
  private val fields: Map[String, Array[Int]] = data.slice(0, firstBlankLine).map(line => {
    val parts = line.split(':').map(_.trim)
    val ranges = parts(1).split(' ').filter(_.contains('-')).flatMap(range => {
      val ends = range.split('-').map(_.toInt)
      ends(0) to ends(1)
    })
    (parts(0), ranges)
  }).toMap
  private val secondBlankLine = data.indexWhere(_ == "", firstBlankLine+1)
  private val mine: Array[Int] = data(secondBlankLine-1).split(',').map(_.toInt)
  private val others: Array[Array[Int]] = data.slice(secondBlankLine+2, data.length).map(_.split(',').map(_.toInt))

  def one: Int = others.map(other => {
    val isInvalid = other.find(value => fields.forall(!_._2.contains(value)))
    isInvalid match {
      case Some(v) => v
      case None => 0
    }
  }).sum

  def findInPossibleNames(possibleNames: List[(Int, Array[String])]): List[(Int, String)] = {
    val currentPossibleNames = possibleNames.head._2
    if (currentPossibleNames.length == 1) {
      val headName = currentPossibleNames(0)
      val tailPossibleNames = possibleNames.tail.map(other => (other._1, other._2.filter(_ != headName)))
      if (tailPossibleNames.isEmpty) List((possibleNames.head._1, headName))
      else (possibleNames.head._1, headName) :: findInPossibleNames(tailPossibleNames)
    } else {
      val firstNames = currentPossibleNames.indices.map(i => (possibleNames(i)._1, currentPossibleNames(i))).toList
      val lastPossibleNames = possibleNames.takeRight(possibleNames.length-currentPossibleNames.length)
        .map(other => (other._1, other._2.filter(!currentPossibleNames.contains(_))))
      firstNames :++ findInPossibleNames(lastPossibleNames)
    }
  }

  def two: Long = {
    val validOthers = others.filter(!_.exists(value => fields.forall(!_._2.contains(value))))
    val possibleNames = mine.indices.map(index => {
      val othersAtIndex = validOthers.map(_(index))
      (index, fields.filter(f => othersAtIndex.forall(f._2.contains)).keys.toArray)
    }).toList.sortBy(_._2.length)
    findInPossibleNames(possibleNames).map(n => if (n._2.startsWith("departure")) mine(n._1).toLong else 1L).product
  }
}