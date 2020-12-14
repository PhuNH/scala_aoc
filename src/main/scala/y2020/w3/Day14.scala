package y2020.w3

import common.Day
import common.Utils._

import scala.collection.mutable
import scala.io.Source

class Day14 extends Day(inputPath(2020, 14), testPath(2020, 14, 1)) {
  private val program: Array[Array[String]] = using(Source.fromResource(inputs(0)))(
    _.getLines().toArray.map(_.split('=').map(_.trim)))

  private def process(version: Int): mutable.Map[Long, Long] = {
    var mask = ""
    var maskValue = 0L
    var supMask = ""
    var supMaskValue = 0L
    val mem = mutable.Map.empty[Long, Long]

    if (version == 1) for (line <- program) {
      if (line(0) == "mask") {
        mask = line(1)
        supMask = mask.map {
          case 'X' => '1'
          case _ => '0'
        }
        supMaskValue = java.lang.Long.parseLong(supMask, 2)
        maskValue = java.lang.Long.parseLong(mask.replace('X', '0'), 2)
      } else {
        val address = line(0).split(Array('[', ']')).apply(1).toLong
        val value: Long = line(1).toLong & supMaskValue | maskValue
        mem += ((address, value))
      }
    } else for (line <- program) {
      if (line(0) == "mask") mask = line(1)
      else {
        val address = line(0).split(Array('[', ']')).apply(1).toInt
        val addressBinaryString = new mutable.StringBuilder(address.toBinaryString.reverse.padTo(36, '0').reverse)
        val xs = new mutable.ArrayBuffer[Int]
        for (i <- 0 until 36)
          if (mask(i) != '0') {
            addressBinaryString.setCharAt(i, mask(i))
            if (mask(i) == 'X') xs += i
          }
        val value = line(1).toLong
        (0 until 1 << xs.length).foreach(n => {
          val xReplacements = n.toBinaryString.reverse.padTo(xs.length, '0').reverse
          xs.indices.foreach(i => addressBinaryString.setCharAt(xs(i), xReplacements(i)))
          mem += ((java.lang.Long.parseLong(addressBinaryString.toString(), 2), value))
        })
      }
    }
    mem
  }

  def one: Long = process(1).values.sum

  def two: Long = process(2).values.sum
}