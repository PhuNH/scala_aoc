package y2018.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day16 extends Day(inputPath(2018, 16)) {
  type Op = (Registers, Int, Int, Int) => Registers
  private val sections: Array[String] = using(Source.fromResource(inputs(0)))(
    _.getLines().mkString("\n").split("\n\n\n\n"))
  private val sampleStrings = sections(0).split("\n\n").map(_.split("\n"))

  case class Registers(data: Array[Int]) {
    override def equals(that: Any): Boolean = that match {
      case that: Registers => data sameElements that.data
      case _ => false
    }
  }
  
  object Registers {
    def op: (Registers, Int, Int) => Registers = (r, c, newCValue) => {
      val clonedData = r.data.clone()
      clonedData(c) = newCValue
      Registers(clonedData)
    }
    def addr: Op = (r, a, b, c) => op(r, c, r.data(a) + r.data(b))
    def addi: Op = (r, a, b, c) => op(r, c, r.data(a) + b)
    def mulr: Op = (r, a, b, c) => op(r, c, r.data(a) * r.data(b))
    def muli: Op = (r, a, b, c) => op(r, c, r.data(a) * b)
    def banr: Op = (r, a, b, c) => op(r, c, r.data(a) & r.data(b))
    def bani: Op = (r, a, b, c) => op(r, c, r.data(a) & b)
    def borr: Op = (r, a, b, c) => op(r, c, r.data(a) | r.data(b))
    def bori: Op = (r, a, b, c) => op(r, c, r.data(a) | b)
    def setr: Op = (r, a, _, c) => op(r, c, r.data(a))
    def seti: Op = (r, a, _, c) => op(r, c, a)
    def gtir: Op = (r, a, b, c) => op(r, c, if (a > r.data(b)) 1 else 0)
    def gtri: Op = (r, a, b, c) => op(r, c, if (r.data(a) > b) 1 else 0)
    def gtrr: Op = (r, a, b, c) => op(r, c, if (r.data(a) > r.data(b)) 1 else 0)
    def eqir: Op = (r, a, b, c) => op(r, c, if (a == r.data(b)) 1 else 0)
    def eqri: Op = (r, a, b, c) => op(r, c, if (r.data(a) == b) 1 else 0)
    def eqrr: Op = (r, a, b, c) => op(r, c, if (r.data(a) == r.data(b)) 1 else 0)
    val ops: Array[Op] = Array(addr, addi, mulr, muli, banr, bani, borr, bori,
      setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)
  }

  private val opcodePossibilities = sampleStrings.map(s => {
    val before = s(0).split(Array('[', ']')).apply(1).split(", ").map(_.toInt)
    val instruction = s(1).split(' ').map(_.toInt)
    val after = s(2).split(Array('[', ']')).apply(1).split(", ").map(_.toInt)
    instruction(0) ->
      Registers.ops.map(f => f(Registers(before), instruction(1), instruction(2), instruction(3)) == Registers(after))
  })

  def one: Int = opcodePossibilities.count(_._2.count(_ == true) >= 3)

  private val tests = sections(1).split("\n").map(_.split(' ').map(_.toInt))
  def two: Int = {
    val intermediateOpcodes = opcodePossibilities.groupMapReduce(_._1)(_._2.zipWithIndex.filter(_._1).map(_._2).toSet)(_ intersect _)
      .toList.sortBy(_._2.size)
    val opcodes = {
      type Temp = List[(Int, Set[Int])]
      def getOpcodes(tempOpcodes: Temp): Temp = tempOpcodes match {
        case h :: Nil => h :: Nil
        case h :: t if t.nonEmpty => h :: getOpcodes(t.map(p => (p._1, p._2.excl(h._2.head))).sortBy(_._2.size))
      }
      getOpcodes(intermediateOpcodes).toMap
    }
    tests.foldLeft(Registers(new Array[Int](4)))((before, instruction) =>
      Registers.ops(opcodes(instruction(0)).head)(before, instruction(1), instruction(2), instruction(3))).data(0)
  }
}