package y2019.w2

import common.Day
import common.Utils._
import y2019.Intcode

class Day2 extends Day(inputPath(2019, 2)) {
  // Opcode - $0 (+4)
  // 99: done
  // 1: *($1) + *($2) -> *($3)
  // 2: *($1) * *($2) -> *($3)
  // 1202 program alarm: 12 -> *($1); 2 -> *($2)

  private val GravityAssist = 19690720

  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private def run1202(codes: Array[Long], noun: Int, verb: Int): Long = {
    codes(1) = noun
    codes(2) = verb
    Intcode(codes).run()
    codes(0)
  }

  def one: Long = run1202(codes.clone(), 12, 2)

  // noun and verb are between 0 and 99, inclusive
  def two: Int = (for {
    noun <- 0 to 99
    verb <- 0 to 99
    if run1202(codes.clone(), noun, verb) == GravityAssist
  } yield 100 * noun + verb).head
}
