package y2019.w4

import common.Day
import common.Utils._

import scala.io.Source
import scala.math.abs

class Day16 extends Day(inputPath(2019, 16), testPath(2019, 16, 1),
  testPath(2019, 16, 2), testPath(2019, 16, 3)) {
  private val signal: Seq[Int] = using(Source.fromFile(inputs(1)))(_.getLines().next().split("").map(_.toInt))
  private val basePattern = Seq(0, 1, 0, -1)
  private val patterns = Array.fill(signal.length)(Array.fill(signal.length)(0))

  private def calOutputAt(input: Seq[Int], dupNum: Int, firstRun: Boolean): Int = {
    if (firstRun) {
      val patDup = basePattern.flatMap(pat => Seq.fill(dupNum)(pat))
      val rotNum = input.length / patDup.length + 1
      val patDupRot = Seq.fill(rotNum)(patDup).flatten
      patDupRot.tail.copyToArray(patterns(dupNum-1))
    }

    abs(input.indices.map(i => input(i) * patterns(dupNum-1)(i)).sum % 10)
  }

  private def runPhase(input: Seq[Int], firstRun: Boolean): Seq[Int] =
    input.indices.map(i => calOutputAt(input, i+1, firstRun))

  @scala.annotation.tailrec
  private def runNPhase(input: Seq[Int], n: Int, firstIndex: Int): Seq[Int] = {
    if (n > 0) {
      val output = runPhase(input, n == firstIndex)
      runNPhase(output, n-1, firstIndex)
    } else input
  }

  def one: Unit = {
    val output = runNPhase(signal, 100, 100)
    println(output.slice(0,8).mkString(""))
  }

  def two: Unit = ()
}