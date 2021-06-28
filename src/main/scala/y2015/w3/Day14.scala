package y2015.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day14 extends Day(inputPath(2015, 14)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def processData(data: Array[String]): Array[(Int, Int, Int)] = data.map(l => {
    val parts = l.split(' ')
    (parts(3).toInt, parts(6).toInt, parts(13).toInt)
  })

  private val params = processData(data)
  private val time = 2503

  private def distAtTime(time: Int, tuple: (Int, Int, Int)): Int = {
    val turnLen = tuple._2 + tuple._3
    val turnCount = time / turnLen
    val timeIntoLastTurn = time % turnLen
    tuple._1 * (turnCount * tuple._2 + tuple._2.min(timeIntoLastTurn))
  }

  def one: Int = params.map(distAtTime(time, _)).max

  def two: Int = {
    val points = Array.fill(params.length)(0)
    for (i <- 1 to time) {
      val dists = params.map(distAtTime(i, _))
      val maxDist = dists.max
      dists.zipWithIndex.filter(_._1 == maxDist).foreach(di => points(di._2) += 1)
    }
    points.max
  }
}