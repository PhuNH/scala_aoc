package y2019.w4

import common.Day
import common.Utils._

import scala.io.Source
import scala.math.abs

class Day16 extends Day(inputPath(2019, 16), testPath(2019, 16, 1),
  testPath(2019, 16, 2), testPath(2019, 16, 3)) {
  private val signal: Array[Int] = using(Source.fromResource(inputs(0)))(_.getLines().next().split("").map(_.toInt))

  private def calOutputAt(input: Seq[Int], outputIndex: Int): Int = {
    val dupNum = outputIndex + 1
    val pluses = input.slice(outputIndex, input.length).sliding(dupNum, 4 * dupNum).flatten.sum
    val minuses = input.slice(outputIndex + 2 * dupNum, input.length).sliding(dupNum, 4 * dupNum).flatten.sum
    abs((pluses - minuses) % 10)
  }

  private def runPhase(input: Array[Int], length: Int = 0): Array[Int] =
    (if (length > 0) 0 until length else input.indices)
      .map(i => calOutputAt(input, i)).toArray

  @scala.annotation.tailrec
  private def runNPhase(input: Array[Int], n: Int, f: (Array[Int], Int) => Array[Int]): Array[Int] = {
    if (n > 1) {
      val output = f(input, 0)
      runNPhase(output, n-1, f)
    } else
      f(input, 8)
  }

  def one: Int = runNPhase(signal, 100, runPhase).mkString("").toInt

  def runPhase2(input: Array[Int], length: Int = 0): Array[Int] = {
    val output = new Array[Int](input.length)
    for (i <- Range.inclusive(input.length-1, 0, -1)) {
      output(i) =
        if (i == input.length-1) input(i)
        else (output(i+1) + input(i)) % 10
    }
    if (length > 0) output.slice(0, length) else output
  }

  def two: Int = {
    //val signal = "03081770884921959731165446850517".split("").map(_.toInt)
    val offset = signal.slice(0, 7).mkString("").toInt
    val input = Array.fill(10000)(signal).flatten.slice(offset, signal.length*10000)
    if (offset >= signal.length) {
      runNPhase(input, 100, runPhase2).mkString("").toInt
    } else 0
  }
}