package y2015.w4

import common.Day
import common.Utils._

import scala.io.Source

class Day24 extends Day(inputPath(2015, 24)) {
  private val data: Array[Int] = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(_.toInt))

  private def findGroups(left: Array[Int], groupSum: Int): Array[Array[Int]] = {
    if (groupSum == 0) Array(Array.empty[Int])
    else if (left.isEmpty || groupSum < 0) Array.empty[Array[Int]]
    else {
      val lastIndexTo = left.zipWithIndex.lastIndexWhere(e => {
        if (groupSum > left.last) left.slice(e._2, left.length).sum >= groupSum
        else e._1 <= groupSum
      })
      if (lastIndexTo < 0) Array.empty[Array[Int]]
      else (0 to lastIndexTo).toArray.flatMap(i => {
        val nextLeft = if (i < left.length-1) left.slice(i+1, left.length) else Array.empty[Int]
        findGroups(nextLeft, groupSum-left(i)).map(left(i) +: _)
      })
    }
  }

  private def findFirstGroupQeIn3(groups: Array[Array[Int]]): Long = {
    val sortedGroupsWithIndices = groups.sortBy(_.length).zipWithIndex
    val firstGroup = sortedGroupsWithIndices.find(gi => {
      sortedGroupsWithIndices.slice(gi._2 + 1, sortedGroupsWithIndices.length - 1).exists(gi2 => {
        gi2._1.forall(!gi._1.contains(_)) && {
          sortedGroupsWithIndices.slice(gi2._2 + 1, sortedGroupsWithIndices.length).exists(gi3 =>
            gi3._1.forall(e => !gi._1.contains(e) && !gi2._1.contains(e)))
        }
      })
    }).get
    firstGroup._1.map(_.toLong).product
  }

  private def findFirstGroupQeIn4(groups: Array[Array[Int]]): Long = {
    val sortedGroupsWithIndices = groups.sortBy(_.length).zipWithIndex
    val firstGroup = sortedGroupsWithIndices.find(gi => {
      sortedGroupsWithIndices.slice(gi._2 + 1, sortedGroupsWithIndices.length - 2).exists(gi2 =>
        gi2._1.forall(!gi._1.contains(_)) &&
          sortedGroupsWithIndices.slice(gi2._2 + 1, sortedGroupsWithIndices.length - 1).exists(gi3 =>
            gi3._1.forall(e => !gi._1.contains(e) && !gi2._1.contains(e)) &&
              sortedGroupsWithIndices.slice(gi3._2 + 1, sortedGroupsWithIndices.length).exists(gi4 =>
              gi4._1.forall(e => !gi._1.contains(e) && !gi2._1.contains(e) && !gi3._1.contains(e)))
          )
      )
    }).get
    firstGroup._1.map(_.toLong).product
  }

  def one: Long = {
    val groupCount = 3
    val groupSum = data.sum / groupCount
    val groups = findGroups(data, groupSum)
    findFirstGroupQeIn3(groups)
  }

  def two: Long = {
    val groupCount = 4
    val groupSum = data.sum / groupCount
    val groups = findGroups(data, groupSum)
    findFirstGroupQeIn4(groups)
  }
}