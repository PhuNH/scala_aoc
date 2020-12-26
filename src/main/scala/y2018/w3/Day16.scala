package y2018.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day16 extends Day(inputPath(2018, 16)) {
  type Op = (Array[Int], Int, Int, Int) => Array[Int]
  private val sections: Array[String] = using(Source.fromResource(inputs(0)))(
    _.getLines().mkString("\n").split("\n\n\n\n"))
  private val sampleStrings = sections(0).split("\n\n").map(_.split("\n"))

  private def op: (Array[Int], Int, Int) => Array[Int] = (r, c, newCValue) => {
    val clonedData = r.clone()
    clonedData(c) = newCValue
    clonedData
  }
  private def addr: Op = (r, a, b, c) => op(r, c, r(a) + r(b))
  private def addi: Op = (r, a, b, c) => op(r, c, r(a) + b)
  private def mulr: Op = (r, a, b, c) => op(r, c, r(a) * r(b))
  private def muli: Op = (r, a, b, c) => op(r, c, r(a) * b)
  private def banr: Op = (r, a, b, c) => op(r, c, r(a) & r(b))
  private def bani: Op = (r, a, b, c) => op(r, c, r(a) & b)
  private def borr: Op = (r, a, b, c) => op(r, c, r(a) | r(b))
  private def bori: Op = (r, a, b, c) => op(r, c, r(a) | b)
  private def setr: Op = (r, a, _, c) => op(r, c, r(a))
  private def seti: Op = (r, a, _, c) => op(r, c, a)
  private def gtir: Op = (r, a, b, c) => op(r, c, if (a > r(b)) 1 else 0)
  private def gtri: Op = (r, a, b, c) => op(r, c, if (r(a) > b) 1 else 0)
  private def gtrr: Op = (r, a, b, c) => op(r, c, if (r(a) > r(b)) 1 else 0)
  private def eqir: Op = (r, a, b, c) => op(r, c, if (a == r(b)) 1 else 0)
  private def eqri: Op = (r, a, b, c) => op(r, c, if (r(a) == b) 1 else 0)
  private def eqrr: Op = (r, a, b, c) => op(r, c, if (r(a) == r(b)) 1 else 0)
  private val ops: Array[Op] = Array(addr, addi, mulr, muli, banr, bani, borr, bori,
    setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)

  private val opcodePossibilities = sampleStrings.map(s => {
    val before = s(0).split(Array('[', ']')).apply(1).split(", ").map(_.toInt)
    val instruction = s(1).split(' ').map(_.toInt)
    val after = s(2).split(Array('[', ']')).apply(1).split(", ").map(_.toInt)
    instruction(0) ->
      ops.map(f => f(before, instruction(1), instruction(2), instruction(3)) sameElements after)
  })

  def one: Int = opcodePossibilities.count(_._2.count(_ == true) >= 3)

  private val tests = sections(1).split("\n").map(_.split(' ').map(_.toInt))
  def two: Int = {
    val intermediateOpcodes =
      opcodePossibilities.groupMapReduce(_._1)(_._2.zipWithIndex.filter(_._1).map(_._2).toSet)(_ intersect _)
        .toList.sortBy(_._2.size)
    val opcodes = {
      type Temp = List[(Int, Set[Int])]
      def getOpcodes(tempOpcodes: Temp): Temp = tempOpcodes match {
        case h :: Nil => h :: Nil
        case h :: t if t.nonEmpty => h :: getOpcodes(t.map(p => (p._1, p._2.excl(h._2.head))).sortBy(_._2.size))
      }
      getOpcodes(intermediateOpcodes).toMap
    }
    tests.foldLeft(new Array[Int](4))((before, instruction) =>
      ops(opcodes(instruction(0)).head)(before, instruction(1), instruction(2), instruction(3)))(0)
  }
}