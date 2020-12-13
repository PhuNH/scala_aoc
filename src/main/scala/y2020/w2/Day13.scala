package y2020.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day13 extends Day(inputPath(2020, 13)) {
  private val notes: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val threshold = notes(0).toInt
  private val busIds = notes(1).split(',').map {
    case "x" => 0
    case a => a.toInt
  }

  def one: Int = {
    val inServiceBusIds = busIds.filterNot(_ == 0)
    val earliestWithId = inServiceBusIds.map(id => (id, ((threshold-1)/id+1)*id)).minBy(_._2)
    earliestWithId._1 * (earliestWithId._2 - threshold)
  }

  def checkX(x: Long, idsIndices: IndexedSeq[(Int, Int)]): IndexedSeq[Long] =
    idsIndices.map(idIndex => (x+idIndex._2) % idIndex._1)

  def two: Long = {
    val idsIndices = busIds.indices.map(i => (busIds(i), i)).filterNot(_._1 == 0)
    /*
     * t = 19a
     * t + 13 = 37b     19a + 50 = 37b + 37     19a + 50 = 37(b+1) = 13(d+1) = 547g    = 17(i-1)
     * t + 19 = 523c    19a + 19 = 523c         19(a+1)  = 523c    = 23(e-1) = 29(f-1) = 41(h-1)
     * t + 37 = 13d     19a + 50 = 13d + 13
     * t + 42 = 23e     19a + 19 = 23e - 23     a+1       = 523 * 23 * 29 * 41 * n = 14302481 * n
     * t + 48 = 29f     19a + 19 = 29f - 29     19a + 50  = 37 * 13 * 547 * 17 * m = 4472819m
     * t + 50 = 547g    19a + 50 = 547g         (14302481n - 1) * 19 + 50 = 4472819 * m
     * t + 60 = 41h     19a + 19 = 41h - 41     271747139 * n + 31 = 4472819 * m
     * t + 67 = 17i     19a + 50 = 17i - 17
     */
    var n = 0L
    var r = 0L
    do {
      n += 1
      r = (271747139 * n + 31) % 4472819
    } while (r != 0)
    val x = 19 * (14302481 * n - 1)
    println(checkX(x, idsIndices).map(_.toString).mkString(" "))
    x
  }
}