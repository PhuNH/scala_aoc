package y2019.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day10 extends Day(inputPath(2019, 10),
  testPath(2019, 10, 1),
  testPath(2019, 10, 2),
  testPath(2019, 10, 3)) {

  private val Asteroid = '#'
  private val BetNo = 200
  type TanLinePoints = Map[Double, Array[(Int, Int)]]
  type Aa = Array[Array[(Int, Int)]]

  private val asteroids =
    using(Source.fromResource(inputs(0)))(_.getLines().toArray).zipWithIndex.flatMap(l => {
      l._1.zipWithIndex.filter(_._1 == Asteroid).map {
        case (_, x) => (x, l._2)
      }
    })

  private def tan(p1: (Int, Int), p2: (Int, Int)): Double =
    if (p2._1 == p1._1)
      if (p2._2 > p1._2) Double.PositiveInfinity else Double.NegativeInfinity
    else (p2._2 - p1._2).toDouble / (p2._1 - p1._1)

  private def tlpFrom(a: (Int, Int), asteroids: Array[(Int, Int)]): (TanLinePoints, TanLinePoints) = {
    val notA = asteroids.filterNot(_ == a)
    val (left, right) = notA.partition(_._1 < a._1)
    (left.groupBy(tan(a, _)), right.groupBy(tan(a, _)))
  }

  private def maxTlp(asteroids: Array[(Int, Int)]): ((Int, Int), TanLinePoints, TanLinePoints) = {
    asteroids.map(a => {
      val tlp = tlpFrom(a, asteroids)
      (a, tlp._1, tlp._2)
    }).maxBy(p => p._2.size + p._3.size)
  }

  private def getLinePoints(tlp: TanLinePoints): Array[Array[(Int, Int)]] =
    tlp.keys.toArray.sorted(Ordering.Double.IeeeOrdering).map(tlp(_))

  private def transform(aa: Aa): Aa = {
    val width = aa.map(_.length).max
    (0 until width).toArray.map(i => {
      aa.map(l => if (i < l.length) l(i) else null)
    })
  }

  private def shotArray(left: TanLinePoints, right: TanLinePoints): Array[(Int, Int)] = {
    val aa = getLinePoints(right).map(l => l.sortWith((a,b) => a._1 < b._1)) ++
      getLinePoints(left).map(l => l.sortWith((a,b) => a._1 > b._1))
    transform(aa).flatten
  }

  // 10.1: 8; 10.2: 33; 10.3: 210
  def one: Int = {
    val p = maxTlp(asteroids)
    p._2.size + p._3.size
  }

  // 10.3: 802
  def two: Int = {
    val p = maxTlp(asteroids)
    val pBet = shotArray(p._2, p._3)(BetNo-1)
    pBet._1 * 100 + pBet._2
  }
}