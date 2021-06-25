package y2015.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day7 extends Day(inputPath(2015, 7)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  case class Wire(formula: String) {
    var signal: Option[Char] = None

    def getSignal(wires: Map[String, Wire]): Char = signal.getOrElse(
      formula.toIntOption match {
        case Some(x) => x.toChar
        case None =>
          val words = formula.split(' ')
          signal = Some(
            if (words.length == 1) wires(words(0)).getSignal(wires)
            else if (words.length == 2) (~wires(words(1)).getSignal(wires)).toChar
            else {
              val left = words(0).toIntOption match {
                case Some(x) => x.toChar
                case None => wires(words(0)).getSignal(wires)
              }
              val right = words(2).toIntOption match {
                case Some(x) => x.toChar
                case None => wires(words(2)).getSignal(wires)
              }
              words(1) match {
                case "AND" => (left & right).toChar
                case "OR" => (left | right).toChar
                case "LSHIFT" => (left << right).toChar
                case _ => (left >> right).toChar
              }
            }
          )
          signal.get
      })
  }

  private def processData(): Map[String, Wire] = data.map(l => {
    val parts = l.split(" -> ")
    (parts(1), Wire(parts(0)))
  }).toMap
  private val wires = processData()

  def one: Int = wires("a").getSignal(wires)

  def two: Int = {
    wires.foreachEntry((s, w) => {
      if (s == "b") w.signal = Some(wires("a").getSignal(wires))
      else w.signal = None
    })
    wires("a").signal = None
    wires("a").getSignal(wires)
  }
}