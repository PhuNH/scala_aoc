package y2015.w3

import common.Day
import common.Utils._

class Day20 extends Day(inputPath(2015, 20)) {
  private val data: Int = 29000000

  def one: Int = {
    val pointOnly = data/10
    val houses: Array[Int] = Array.fill(pointOnly)(0)
    (1 to pointOnly).foreach(i => Range(i, pointOnly, i).foreach(houses(_) += i))
    houses.indexWhere(_ * 10 >= data)
  }

  def two: Int = {
    val pointOnly = data/11
    val houses = Array.fill(pointOnly)(0)
    (1 to pointOnly).foreach(i => Range(i, pointOnly.min(i*51), i).foreach(houses(_) += i))
    houses.indexWhere(_ * 11 >= data)
  }
}