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

  private def parse(string: String): Long = {
    val expressionString = string.split(' ').mkString("")
    var i = 0
    var operand: Option[Long] = None
    var operator: Option[Char] = None
    while (i < expressionString.length) {
      if (expressionString(i) >= '0' && expressionString(i) <= '9')
        operand = Some(
          if (operand.isEmpty) expressionString(i).toString.toLong
          else
            if (operator.get == '+') operand.get + expressionString(i).toString.toLong
            else operand.get * expressionString(i).toString.toLong
        )
      else if (expressionString(i) == '*' || expressionString(i) == '+') operator = Some(expressionString(i))
      else if (expressionString(i) == '(') {
        val until = findEquivParens(expressionString, i)
        val subExpressionString = expressionString.substring(i+1, until)
        operand = Some(
          if (operand.isEmpty) parse(subExpressionString)
          else
            if (operator.get == '+') operand.get + parse(subExpressionString)
            else operand.get * parse(subExpressionString)
        )
        i = until
      }
      i += 1
    }
    operand.get
  }

  def one: Long = expressionStrings.map(parse).sum

  def two: Unit = {}
}