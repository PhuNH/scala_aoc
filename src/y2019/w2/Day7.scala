package y2019.w2

import common.Day
import common.Utils._
import y2019.Intcode

class Day7 extends Day(inputPath(2019, 7)) {
  private val codes = Intcode.readCodes(inputs.head)

  private def createAmplifiers(phaseSettings: Array[Int]) = {
    val amplifiers = new Array[Intcode](5)
    for (i <- 0 to 4) {
      amplifiers(i) = Intcode(codes)
      amplifiers(i).setInput(phaseSettings(i))
      amplifiers(i).run()
    }
    amplifiers
  }

  private def loopAmplify(phaseSettings: Array[Int]): Long = {
    val amplifiers = createAmplifiers(phaseSettings)
    val inout = new Array[Long](5)
    inout(0) = 0

    while (!amplifiers(4).isFinished) {
      for (i <- 0 to 4) {
        amplifiers(i).setInput(inout(i))
        inout(if (i < 4) i+1 else 0) = amplifiers(i).run()
      }
    }
    inout(0)
  }

  def one: Long = (0 to 4).toArray.permutations.toList.map(loopAmplify).max

  def two: Long = (5 to 9).toArray.permutations.toList.map(loopAmplify).max
}