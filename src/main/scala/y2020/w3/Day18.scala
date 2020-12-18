package y2020.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day18 extends Day(inputPath(2020, 18)) {
  private val expressionStrings: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def findEquivParens(expressionString: String, open: Int): Int = {
    var i = open+1
    var count = 0
    while (i < expressionString.length) {
      if (expressionString(i) == '(') count += 1
      else if (expressionString(i) == ')') count -= 1
      if (count == -1) return i
      i += 1
    }
    -1
  }

  private def evaluate(char: Char, index: Int, subExpressions: Map[Int, Long]): Long = char match {
    case '_' => subExpressions(index)
    case _ => char.toString.toLong
  }

  private def parseNoParens(string: String, mode: Int, subExpressions: Map[Int, Long]): Long = mode match {
    case 0 =>
      var i = 0
      var operand: Option[Long] = None
      var operator: Char = ' '
      while (i < string.length) {
        if (string(i) >= '0' && string(i) <= '9' || string(i) == '_') {
          val that = evaluate(string(i), i, subExpressions)
          operand = Some(
            if (operand.isEmpty) that
            else
              if (operator == '+') operand.get + that
              else operand.get * that
          )
        } else if ("*+".contains(string(i))) operator = string(i)
        i += 1
      }
      operand.get
    case _ =>
      val expressionBuffer = new StringBuffer(string)
      var plusIndex = expressionBuffer.indexOf("+")
      var sumExpressions = subExpressions
      while (plusIndex > -1) {
        val left = evaluate(expressionBuffer.charAt(plusIndex-1), plusIndex-1, sumExpressions)
        val right = evaluate(expressionBuffer.charAt(plusIndex+1), plusIndex+1, sumExpressions)
        expressionBuffer.replace(plusIndex-1, plusIndex+2, "_")
        sumExpressions = sumExpressions.map {
          case (i, l) if i > plusIndex-1 => (i-2, l)
          case p => p
        }
        sumExpressions = sumExpressions + ((plusIndex-1, left+right))
        plusIndex = expressionBuffer.indexOf("+")
      }
      expressionBuffer.toString.zipWithIndex.filterNot(_._1 == '*').map(m => evaluate(m._1, m._2, sumExpressions)).product
  }

  private def parse(string: String, mode: Int): Long = {
    val expressionBuffer = new StringBuffer(string.split(' ').mkString(""))
    var parenIndex = expressionBuffer.indexOf("(")
    var subExpressions = Map.empty[Int, Long]
    while (parenIndex > -1) {
      val until = findEquivParens(expressionBuffer.toString, parenIndex)
      subExpressions = subExpressions + ((parenIndex, parse(expressionBuffer.substring(parenIndex+1, until), mode)))
      expressionBuffer.replace(parenIndex, until+1, "_")
      parenIndex = expressionBuffer.indexOf("(")
    }
    parseNoParens(expressionBuffer.toString, mode, subExpressions)
  }

  def one: Long = expressionStrings.map(parse(_, 0)).sum

  def two: Long = expressionStrings.map(parse(_, 1)).sum
}