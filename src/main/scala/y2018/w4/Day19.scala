package y2018.w4

import common.Day
import common.Utils._
import y2018.ElfCode

import scala.io.Source

class Day19 extends Day(inputPath(2018, 19)) {
  private val code: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val instructions = code.tail
  private val ip_bound = code.head.split(' ')(1).toInt

  def one: Int = {
    val registers = Array.fill(6)(0)
    var ip = 0
    while (ip < instructions.length)
      ip = ElfCode.runInstruction(instructions, ip_bound, registers, ip)
    registers(0)
  }

  def two: Int = {
    // 10551424 = 1024 + (27*28+29)*13440
    (1 to 2637856).filter(10551424 % _ == 0).sum + 10551424 + 5275712
  }
}