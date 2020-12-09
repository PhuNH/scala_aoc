package y2019

import common.Utils.using

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Intcode(inputCodes: Array[Long]) {
  private val codes = new Array[Long](inputCodes.length * 10)
  inputCodes.copyToArray(codes)
  private var ip = 0
  private var relBase = 0
  private var input = 0L
  private var outputs = ArrayBuffer(0L)
  private var ined = true
  private var state = Intcode.Paused

  private def parseInstruction(instr: Int): (Int, List[Int]) = {
    val opcode = instr % 100
    val paramModeStr = (instr / 100).toString
    val paramModes = (
      if (paramModeStr.length < 3) "0" * (3 - paramModeStr.length) + paramModeStr
      else paramModeStr).reverse.map(_.toString.toInt).toList
    (opcode, paramModes)
  }

  private def read(offset: Int, paramModes: List[Int]): Long =
    if (paramModes(offset-1) == Intcode.Immediate) codes(ip+offset)
    else if (paramModes(offset-1) == Intcode.Position) codes(codes(ip+offset).toInt)
    else codes(relBase + codes(ip+offset).toInt)

  private def write(offset: Int, paramModes: List[Int], value: Long) {
    if (paramModes(offset-1) == Intcode.Position) codes(codes(ip+offset).toInt) = value
    else codes(relBase + codes(ip+offset).toInt) = value
  }

  private def runInstruction(): Unit = {
    val (opcode, paramModes) = parseInstruction(codes(ip).toInt)
    opcode match {
      // 2, 1-2
      case Intcode.Add =>
        write(3, paramModes, read(1, paramModes) + read(2, paramModes))
        ip += 4
      case Intcode.Mul =>
        write(3, paramModes, read(1, paramModes) * read(2, paramModes))
        ip += 4
      // 5.1, 3-4
      case Intcode.In =>
        if (!ined) {
          write(1, paramModes, input)
          ip += 2
          ined = true
        } else
          state = Intcode.Paused
      case Intcode.Out =>
        outputs(0) += 1
        outputs += read(1, paramModes)
        ip += 2
        //state = Intcode.Paused
      // 5.2 5-8: Int
      case Intcode.JiT =>
        if (read(1, paramModes) != 0) ip = read(2, paramModes).toInt
        else ip += 3
      case Intcode.JiF =>
        if (read(1, paramModes) == 0) ip = read(2, paramModes).toInt
        else ip += 3
      case Intcode.Lt =>
        write(3, paramModes,
          if (read(1, paramModes) < read(2, paramModes)) 1
          else 0)
        ip += 4
      case Intcode.Eq =>
        write(3, paramModes,
          if (read(1, paramModes) == read(2, paramModes)) 1
          else 0)
        ip += 4
      // 9.1, 9
      case Intcode.Rel =>
        val diff = read(1, paramModes).toInt
        relBase += diff
        ip += 2
      // 2, 99
      case Intcode.Halt =>
        state = Intcode.Stopped
        ip += 1
    }
  }

  def run(outputs: ArrayBuffer[Long] = ArrayBuffer(0L)): Unit = {
    state = Intcode.Running
    this.outputs = outputs

    do {
      runInstruction()
    } while (state == Intcode.Running)
  }

  def setInput(input: Long = 0): Boolean = {
    if (state == Intcode.Paused) {
      this.input = input
      ined = false
      true
    } else
      false
  }

  def getState: Int = state
}

object Intcode {
  val Running = 0
  val Paused = 1
  val Stopped = 2

  val Add = 1
  val Mul = 2
  val In = 3
  val Out = 4
  val JiT = 5
  val JiF = 6
  val Lt = 7
  val Eq = 8
  val Rel = 9
  val Halt = 99

  val Position = 0
  val Immediate = 1
  val Relative = 2

  val IdAc = 1
  val IdTrc = 5
  val TestBoost = 1
  val SensorBoost = 2

  def apply(codes: Array[Long]): Intcode = new Intcode(codes)

  def readCodes(filePath: String): Array[Long] = using(Source.fromResource(filePath))(
    _.getLines().next().split(",").map(_.toLong)
  )

  def runProgram(codes: Array[Long], input: Long): Unit = {
    val program = Intcode(codes)
    program.setInput(input)
    val outputs = ArrayBuffer(0L)
    program.run(outputs)
    println(outputs.slice(1, outputs(0).toInt+1).mkString(" "))
  }
}
