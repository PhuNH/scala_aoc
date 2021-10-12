package y2019.w4

import common.Day
import common.Utils._
import y2019.{AsciiInterface, Intcode}

import scala.collection.mutable.ArrayBuffer

class Day21 extends Day(inputPath(2019, 21)) {
  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private def runSpringscript(asciiCodeString: String): Unit = {
    val compiler = Intcode(codes)
    val asciiCode = AsciiInterface.string2Ascii(asciiCodeString)
    val springscript = Springscript(compiler, asciiCode)
    springscript.run()
  }

  def one: Unit = {
    val asciiCodeString =
      """NOT B J
        |NOT C T
        |OR T J
        |AND D J
        |NOT A T
        |OR T J
        |WALK
        |""".stripMargin
    runSpringscript(asciiCodeString)
  }

  def two: Unit = {
    val asciiCodeString =
      """NOT B J
        |NOT C T
        |OR T J
        |AND D J
        |AND H J
        |NOT A T
        |OR T J
        |RUN
        |""".stripMargin
    runSpringscript(asciiCodeString)
  }
}

private case class Springscript(compiler: Intcode, asciiCode: IndexedSeq[Int]) {
  def run(): Unit = {
    val outputs = ArrayBuffer(0L)
    compiler.run(outputs)
    AsciiInterface.inputSequence(compiler, outputs, asciiCode)

    print(AsciiInterface.outputs2String(outputs.tail.init))
    if (outputs.tail.last > 255) println(outputs.tail.last)
    else print(outputs.tail.last.toChar)
  }
}