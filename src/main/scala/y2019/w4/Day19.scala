package y2019.w4

import common.Day
import common.Utils._
import y2019.Intcode

import scala.collection.mutable.ArrayBuffer

class Day19 extends Day(inputPath(2019, 19)) {
  private val space: ArrayBuffer[ArrayBuffer[Long]] = ArrayBuffer.fill(50)(ArrayBuffer.fill(50)(2))

  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private def getLoc(i: Int, j: Int): Long = {
    if (j >= space(0).length)
      space.map(_.appendAll(Array.fill(50)(2L)))
    if (i >= space.length)
      space.appendAll(ArrayBuffer.fill(50)(ArrayBuffer.fill(space(0).length)(2)))
    if (space(i)(j) == 2) {
      val program = Intcode(codes)
      val outputs = ArrayBuffer(0L)
      program.setInput(j)
      program.run()
      program.setInput(i)
      program.run(outputs)
      space(i)(j) = outputs(1)
    }
    space(i)(j)
  }

  def one: Long = {
    (for {
      i <- 0 until 50
      j <- 0 until 50
    } yield getLoc(i, j)).sum
  }

  private def isRoot(i: Int, j: Int): Boolean = {
    @scala.annotation.tailrec
    def check(offset: Int, rowOrCol: Boolean): Boolean = {
      if (offset >= 100) true
      else {
        val locVal = if (rowOrCol) getLoc(i, j+offset) else getLoc(i+offset, j)
        if (locVal == 0) false
        else check(offset+1, rowOrCol)
      }
    }

    check(1, rowOrCol = true) && check(1, rowOrCol = false)
  }

  def two: Long = {
    @scala.annotation.tailrec
    def checkRow(i: Int, j: Int, passed: Boolean): Int = {
      if (j >= 1000 || (getLoc(i, j) == 0 && passed)) -1
      else
        if (getLoc(i, j) == 1)
          if (isRoot(i, j)) j
          else checkRow(i, j+1, passed = true)
        else checkRow(i, j+1, passed)
    }

    for (i <- 0 until 1000) {
      val j = checkRow(i, 0, passed = false)
      if (j >= 0) {
//        for (i <- space.indices)
//          for (j <- 0 until space(0).length-10) {
//            if (j == 0) println()
//            print(space(i)(j))
//          }
        return j*10000+i
      }
    }
    -1
  }
}
