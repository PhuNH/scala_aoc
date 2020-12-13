package y2020.w2

import common.{Coords, Day}
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day12 extends Day(inputPath(2020, 12)) {
  case class Instruction(action: Char, value: Int) {
    def this(s: String) = this(s(0), s.substring(1).toInt)
  }
  private val actionToDirection =
    Map('N' -> Coords(-1, 0), 'S' -> Coords(1, 0), 'E' -> Coords(0, 1), 'W' -> Coords(0, -1))
  private val actionToAngle = Map('L' -> 1, 'R' -> -1)

  private val instructions: Array[Instruction] = using(Source.fromResource(inputs(0)))(
    _.getLines().toArray.map(new Instruction(_)))

  private def instruct(direction: Coords, location: Coords, version: Int = 0): Int = {
    @tailrec
    def processInstruction(instruction: Instruction): Unit = instruction.action match {
      case a if "NSEW".contains(a) =>
        val whatToUpdate = if (version == 0) location else direction
        whatToUpdate += actionToDirection(a) * instruction.value
      case a if "LR".contains(a) => instruction.value match {
        case 90 => direction.updateWith(direction.swap * Coords(-1, 1) * actionToAngle(a))
        case 180 => direction *= -1
        case 270 => processInstruction(Instruction("LR".filterNot(_ == a).head, 90))
      }
      case 'F' => location.updateWith(direction * instruction.value + location)
    }

    instructions.foreach(processInstruction)
    location.selfMhtDist
  }

  def one: Int = instruct(Coords(0, 1), Coords(0, 0))

  def two: Int = instruct(Coords(-1, 10), Coords(0, 0), 1)
}