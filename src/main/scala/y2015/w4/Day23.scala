package y2015.w4

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day23 extends Day(inputPath(2015, 23)) {
  private val program: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val manipulations = Array("hlf", "tpl", "inc")
  private val ifJumps = Array("jie", "jio")
  private val maxValue = math.pow(2, 32).toLong

  @tailrec
  private def parse(program: Array[String], registers: Array[Long], ip: Int): Unit = if (ip < program.length) {
    val parts = program(ip).split(' ')
    if (manipulations.contains(parts(0))) {
      val index = parts(1).toCharArray.head - 'a'
      registers(index) = parts(0) match {
        case "hlf" => registers(index) / 2
        case "tpl" =>
          val rawValue = registers(index) * 3
          if (rawValue < maxValue) rawValue else rawValue - maxValue
        case _ => registers(index) + 1
      }
      parse(program, registers, ip+1)
    } else if (parts(0) == "jmp") parse(program, registers, ip + parts(1).toInt)
    else if (ifJumps.contains(parts(0))) {
      val index = parts(1).head - 'a'
      if (parts(0) == "jie") parse(program, registers, ip + (if (registers(index) % 2 == 0) parts(2).toInt else 1))
      else parse(program, registers, ip + (if (registers(index) == 1) parts(2).toInt else 1))
    }
  }

  def one: Long = {
    val registers = Array.fill(2)(0L)
    parse(program, registers, 0)
    registers(1)
  }

  def two: Long = {
    val registers = Array(1L, 0)
    parse(program, registers, 0)
    registers(1)
  }
}