package y2019.w3

import common.Day
import common.Utils._
import y2019.Intcode

// Memory is added, so Day2 is not supported anymore
class Day9 extends Day(inputPath(2019, 9), "104,1125899906842624,99") {
  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  def one: Unit = Intcode.runProgram(codes, Intcode.TestBoost)

  def two: Unit = Intcode.runProgram(codes, Intcode.SensorBoost)
}