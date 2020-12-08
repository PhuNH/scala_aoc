package y2020.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day8 extends Day(inputPath(2020, 8)) {
  private val source: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def runInstruction(source: Array[String], ip: Int, acc: Int): (Int, Int) = {
    val opArg = source(ip).split(' ')
    val arg = opArg(1).toInt
    opArg(0) match {
      case "acc" => (ip+1, acc+arg)
      case "jmp" => (ip+arg, acc)
      case "nop" => (ip+1, acc)
    }
  }

  @scala.annotation.tailrec
  private def runProgram(source: Array[String], ip: Int, acc: Int, executed: Set[Int]): (Int, Int) = {
    if (ip >= source.length) (0, acc)
    else if (executed.contains(ip)) (1, acc)
    else {
      val (newIp, newAcc) = runInstruction(source, ip, acc)
      runProgram(source, newIp, newAcc, executed+ip)
    }
  }

  def one: Int = runProgram(source, 0, 0, Set.empty[Int])._2

  def two: Int = {
    var i = 0
    do {
      if (source(i).startsWith("jmp")) {
        val copied = source.clone()
        copied(i) = copied(i).replace("jmp", "nop")
        val result = runProgram(copied, 0, 0, Set.empty[Int])
        if (result._1 == 0) return result._2
      } else if (source(i).startsWith("nop")) {
        val copied = source.clone()
        copied(i) = copied(i).replace("nop", "jmp")
        val result = runProgram(copied, 0, 0, Set.empty[Int])
        if (result._1 == 0) return result._2
      }
      i += 1
    } while (true)
    1
  }
}
