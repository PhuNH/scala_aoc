package y2019.w2

import common.Day
import common.Utils._
import y2019.Intcode

class Day5 extends Day(inputPath(2019, 5)) {
  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  def one: Unit = Intcode.runProgram(codes, Intcode.IdAc)

  def two: Unit = Intcode.runProgram(codes, Intcode.IdTrc)
}
