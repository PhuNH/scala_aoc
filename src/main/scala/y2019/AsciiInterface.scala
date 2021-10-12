package y2019

import common.ExpandableGrid

import scala.collection.mutable.ArrayBuffer

object AsciiInterface {
  def ascii2Grid(outputs: ArrayBuffer[Long]): ExpandableGrid[Int] = {
    @scala.annotation.tailrec
    def splitData(data: ArrayBuffer[Int], acc: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
      val splitIndex = data.indexOf('\n')
      if (splitIndex == -1) {
        if (data.nonEmpty) acc += data else acc
      } else {
        val splitted = data.splitAt(splitIndex)
        splitData(splitted._2.tail, if (splitted._1.nonEmpty) acc += splitted._1 else acc)
      }
    }

    val data = splitData(outputs.map(_.toInt), ArrayBuffer.empty[ArrayBuffer[Int]])
    val grid = ExpandableGrid[Int]()
    grid.setData(data)
    grid
  }

  def string2Ascii(string: String): IndexedSeq[Int] = string.map(c => c.toInt)

  def outputs2String(outputs: ArrayBuffer[Long]): String = outputs.map(_.toChar).mkString("")

  def inputSequence(program: Intcode, outputs: ArrayBuffer[Long] = ArrayBuffer(0L), seq: IndexedSeq[Int]): Unit = {
    for (i <- seq) {
      program.setInput(i)
      program.run(outputs)
    }
  }

  def inputString(program: Intcode, outputs: ArrayBuffer[Long] = ArrayBuffer(0L), string: String): Unit = {
    print(string)
    inputSequence(program, outputs, string2Ascii(string))
  }
}
