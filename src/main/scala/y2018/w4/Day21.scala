package y2018.w4

import common.Day
import common.Utils._
import y2018.ElfCode

import scala.collection.mutable
import scala.io.Source

class Day21 extends Day(inputPath(2018, 21)) {
  private val code: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val instructions = code.tail
  private val ip_bound = code.head.split(' ')(1).toInt

  private def runTest(): Int = {
    val registers = Array.fill(6)(0)
    var ip = 0
    while (ip < instructions.length)
      ip = ElfCode.runInstruction(instructions, ip_bound, registers, ip)
    registers(0)
  }

  private def runOnce(outerA4: Int): Int = {
    var (a1, a4) = (12772194, outerA4)
    while (true) {
      a1 = (((a1 + (a4 & 255)) & 16777215) * 65899) & 16777215
      if (a4 < 256) return a1
      a4 /= 256
    }
    0
  }

  private def runMultiple(): Int = {
    val seen = mutable.Set.empty[Int]
    var last = 0
    var (a1, a4) = (0, 0)
    while (true) {
      a4 = a1 | 65536
      a1 = runOnce(a4)
      if (!seen.contains(a1)) {
        seen.add(a1)
        last = a1
      }
      else return last
    }
    0
  }

  def one: Int = runOnce(65536)

  def two: Int = runMultiple()
}