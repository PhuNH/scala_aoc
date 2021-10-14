package y2018.w4

import common.Day
import common.Utils._

import scala.io.Source

class Day19 extends Day(inputPath(2018, 19)) {
  private val code: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val instructions = code.tail
  private val ip_bound = code.head.split(' ')(1).toInt

  private def runInstruction(instructions: Array[String], registers: Array[Int], ip: Int): Int = {
    val opcode::operandList = instructions(ip).split(' ').toList
    val operands = operandList.map(_.toInt).toArray
    registers(ip_bound) = ip
    val value = opcode match {
      case "addr" => registers(operands(0)) + registers(operands(1))
      case "addi" => registers(operands(0)) + operands(1)
      case "mulr" => registers(operands(0)) * registers(operands(1))
      case "muli" => registers(operands(0)) * operands(1)
      case "banr" => registers(operands(0)) & registers(operands(1))
      case "bani" => registers(operands(0)) & operands(1)
      case "borr" => registers(operands(0)) | registers(operands(1))
      case "bori" => registers(operands(0)) | operands(1)
      case "setr" => registers(operands(0))
      case "seti" => operands(0)
      case "gtir" => if (operands(0) > registers(operands(1))) 1 else 0
      case "gtri" => if (registers(operands(0)) > operands(1)) 1 else 0
      case "gtrr" => if (registers(operands(0)) > registers(operands(1))) 1 else 0
      case "eqir" => if (operands(0) == registers(operands(1))) 1 else 0
      case "eqri" => if (registers(operands(0)) == operands(1)) 1 else 0
      case "eqrr" => if (registers(operands(0)) == registers(operands(1))) 1 else 0
      case _ => 0
    }
    registers(operands(2)) = value
    registers(ip_bound) + 1
  }

  def one: Int = {
    val registers = Array.fill(6)(0)
    var ip = 0
    while (ip < instructions.length)
      ip = runInstruction(instructions, registers, ip)
    registers(0)
  }

  def two: Int = {
    // 10551424 = 1024 + (27*28+29)*13440
    (1 to 2637856).filter(10551424 % _ == 0).sum + 10551424 + 5275712
  }
}