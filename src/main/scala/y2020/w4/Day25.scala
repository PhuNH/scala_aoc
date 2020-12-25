package y2020.w4

import common.Day
import common.Utils._

import scala.io.Source

class Day25 extends Day(inputPath(2020, 25), testPath(2020, 25, 1)) {
  private val publicKeys: Array[Int] = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(_.toInt))

  private def getLoopSize(publicKey: Int): Int = {
    val divisor = 20201227
    val subjectNumber = 7
    var loopSize = 0
    var value = 1L
    while (value != publicKey) {
      value = (value * subjectNumber) % divisor
      loopSize += 1
    }
    loopSize
  }

  private def getEncryptionKey(loopSize: Int, publicKey: Int): Long = {
    val divisor = 20201227
    var value = 1L
    var i = 0
    while (i < loopSize) {
      value = (value * publicKey) % divisor
      i += 1
    }
    value
  }

  def one: Long = {
    val loopSize = getLoopSize(publicKeys(0))
    getEncryptionKey(loopSize, publicKeys(1))
  }

  def two: Unit = {}
}